(in-package :armish)

;; asm spaces
(defparameter *asm-spaces* (make-hash-table))
(defparameter *current-asm-space* nil)
(defparameter *current-asm-block* nil)

(defclass asm-space ()
  ((blocks :accessor blocks-of :initform (make-hash-table) :initarg :blocks)))

(defclass asm-block ()
  ((base-address :accessor base-address-of :initform 0 :initarg :base-address)
   (fns :accessor fns-of :initform (make-hash-table) :initarg :fns)
   (labels :accessor labels-of :initform (make-hash-table) :initarg :labels)))

(defun clear-current-block ()
  (clrhash (fns-of *current-asm-block*)))

(defun %def-asm-space (name)
  (setf (gethash name *asm-spaces*) (make-instance 'asm-space)))

(defmacro def-asm-space (name)
  `(%def-asm-space ',name))

(defun get-asm-space (name)
  (gethash name *asm-spaces*))

(defun %in-asm-space (name)
  (setf *current-asm-space* (get-asm-space name)))

(defmacro in-asm-space (name)
  `(%in-asm-space ',name))

(defun %def-block (name &key in (base-address 0))
  (setf (gethash name (blocks-of (get-asm-space in)))
        (make-instance 'asm-block
                       :base-address base-address)))

(defmacro def-block (name &key in (base-address 0))
  `(%def-block ',name :in ',in :base-address ,base-address))

(defun get-block (name &key in)
  (let ((asm-space (if in
                       in
                       *current-asm-space*)))
    (gethash name (blocks-of asm-space))))

(defun %in-block (name &key in)
  (setf *current-asm-block* (get-block name :in in)))

(defmacro in-block (name &key in)
  `(%in-block ',name :in ,in))

(defmacro def-space-n-blocks (space-name &body block-specs)
  `(progn
     (def-asm-space ,space-name)
     ,@(loop for spec in block-specs
          collect (if (symbolp spec)
                      `(def-block ,spec :in ,space-name)
                      `,(append '(def-block) (list (car spec)) (cdr spec) `(:in ,space-name))))))

(def-space-n-blocks armish-user
  user-block)

(in-asm-space armish-user)
(in-block user-block)

;; allow macro's in assembly
;; don't want to take away any of the power of macro's
(defparameter *asm-macros* '())

(defun add-asm-macro (symbol)
  (setf *asm-macros* (adjoin symbol *asm-macros*)))

(defun asm-macro-p (symbol)
  (member symbol *asm-macros*))

(defmacro def-asm-macro-lite (name &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (progn
       (add-asm-macro ',name)
       (defmacro ,name ()
         ',body))))

(defmacro def-asm-macro (name args &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (progn
       (add-asm-macro ',name)
       (defmacro ,name ,args
         ,@body))))

;; assembly snippets gathering machinery
(defun gather-code (&rest args)
  (gather args))

(defun gather (&rest instr-lists)
  (apply #'append instr-lists))

(defun emit (&rest atoms)
  (remove 'nil atoms))

(defmacro set-asm-init-routines (&body forms)
  `(set-asm-init-fn (lambda ()
                      ,@forms)))

(let ((init-fn (lambda ()
                 (warn "no init-fn defined"))))

  (defun set-asm-init-fn (fn)
    (setf init-fn fn))
  
  (defun emit-init-fn ()
    (funcall init-fn)))

(defmacro set-asm-final-routines (&body forms)
  `(set-asm-final-fn (lambda ()
                       ,@forms)))

(let ((final-fn (lambda ()
                  (warn "no final-fn defined"))))

  (defun set-asm-final-fn (fn)
    (setf final-fn fn))
  
  (defun emit-final-fn ()
    (funcall final-fn)))

(defun emit-arm-fns (&optional (asm-block *current-asm-block*))
  (append (emit-init-fn)
          (loop for init being the hash-value in (fns-of asm-block)
             append (funcall init))
          (emit-final-fn)))

(defmacro def-asm-fn-raw (name args &body body)
  `(setf (gethash ',name (fns-of *current-asm-block*))
         (lambda ,args
           ,@body)))

(defmacro def-asm-fn (name &body body)
  `(setf (gethash ',name (fns-of *current-asm-block*))
         (lambda ()
           (emit-asm
            ,(intern  (symbol-name name) :keyword)
            ,@body))))

;; syntactic checkers of allowable registers. Gets dirty-fied by the arm asm
;; convention of post- and prefix symbols
(defun multi-reg-p-checker (cleaved-regs)
  (loop
     for single-reg in cleaved-regs
     for clean-single-reg = (intern (symbol-name single-reg) 'armish)
     for regs-length = (length cleaved-regs)
     for regs-list = (when (eq regs-length 2)
                       t)
     when (not (if regs-list
                   (regp clean-single-reg)
                   (any-reg-p clean-single-reg)))
     return nil
     finally (return t)))

(defun any-or-mr-reg-p (regs)    
  "check if reg or list of regs are indeed allowable regs; coproc regs included
   used in assembly variable checker"
  (let* ((string-regs (symbol-name regs))
         (clean-regs (intern (symbol-name regs) 'armish)))
    (cond
      ((equal string-regs "^") t)
      ((equal string-regs "!") t)
      ((string-equal (subseq string-regs (- (length string-regs) 1)) "!")
       (bang-reg-p clean-regs))
      ((string-equal (subseq string-regs 0 1) "-")
       (any-reg-p clean-regs))
      (t
       (multi-reg-p-checker (expand-mr-token-list
                             clean-regs
                             #'identity
                             #'identity))))))

(defun maybe-mend-multi-reg (reg-list)
  (if (eql (length reg-list) 1)
      (car reg-list)
      (concat-symbol (car reg-list) #\_ (cadr reg-list))))

(defun mend-bang-reg (val)
  (concat-symbol val '!))

(defun mend-min-reg (val)
  (concat-symbol '- val))

(defun reg-min-bangify (symbol)
  (let* ((string-regs (symbol-name symbol)))
    (cond
      ((string-equal (subseq string-regs (- (length string-regs) 1)) "!")
       `(mend-bang-reg ,(intern (subseq string-regs 0 (- (length string-regs) 1)))))
      ((string-equal (subseq string-regs 0 1) "-")
       `(mend-min-reg ,(intern (subseq string-regs 1))))
      (t `(maybe-mend-multi-reg (list ,@(expand-mr-token-list
                                         symbol
                                         #'identity
                                         #'identity)))))))
      
;; emit assembly
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun escape-assembly-constants (symbol)
    (if (any-or-mr-reg-p symbol)
        `',symbol
        (reg-min-bangify symbol))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %emit-asm (instrs)
    (loop for item in instrs
       collect (etypecase item
                 (cons (if (special-syntactic-asm-form-p item)
                           (handle-special-syntactic-asm-form item)
                           (append '(list) (%emit-asm item))))
                 (keyword item)
                 (symbol (escape-assembly-constants item))
                 (character (aref (string-to-octets (format nil "~a" item) *string-encoding*) 0))
                 (number item)
                 (string item)))))

(defmacro emit-asm (&rest pre-macro-rem-instrs)
  (let ((instrs (apply #'append (loop for thing in pre-macro-rem-instrs
                                   collect (typecase thing
                                             (cons (if (asm-macro-p (car thing))
                                                       (macroexpand thing)
                                                       (list thing)))
                                             (symbol (if (asm-macro-p thing)
                                                         (macroexpand (list thing))
                                                         (list thing)))
                                             (t (list thing)))))))
    `(list
      ,@(loop for expr in instrs
           collect (etypecase expr
                     (cons (if (special-syntactic-asm-form-p expr)
                               (handle-special-syntactic-asm-form expr)
                               (append (list 'list `',(car expr)) 
                                       (if (cdr expr) (%emit-asm (cdr expr))))))
                     (keyword expr)
                     (symbol `',expr)
                     (string expr))))))

(defun handle-special-syntactic-asm-form (form)
  (let ((internal-form-name (intern (symbol-name (car form)) 'armish)))
    ;; these forms are handled a bit naively. change this function and
    ;; the emit-asm clan to mangle their outcome recursively through emit-asm again
    (if (directive-form-p form)
        (handle-directive-form form)
        (ecase internal-form-name
          (ea
           (cadr form))
          (ia
           `(quote ,(cadr form)))
          (address
           `(quote ,form))
          (otherwise
           (error "don't know how to handle special syntactic asm form ~a" form))))))

(defun special-syntactic-asm-form-p (form)
  (let ((internal-form (intern (symbol-name (car form)) 'armish)))
    (if (directive-form-p form)
        t
        (case internal-form
          ((ea ia address)
           t)
          (otherwise
           nil)))))