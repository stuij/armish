(in-package :armish)

;; asm spaces
(defparameter *asm-spaces* (make-hash-table))
(defparameter *asm-blocks* (make-hash-table))
(defparameter *current-asm-space* nil)
(defparameter *current-asm-block* nil)
(defparameter *current-asm-cluster* nil)

;; (clrhash *asm-spaces*)
;; (clrhash *asm-blocks*)

;; spaces
(defclass asm-space ()
  ((clusters :accessor clusters-of :initform (make-hash-table) :initarg :clusters)))

(defun %def-asm-space (name)
  (setf (gethash name *asm-spaces*) (make-instance 'asm-space)))

(defmacro def-asm-space (name)
  `(%def-asm-space ',name))

(defun get-asm-space (name)
  (aif (gethash name *asm-spaces*)
       it
       (error "asm space ~a not found" name)))

(defun %in-asm-space (name)
  (setf *current-asm-space* (get-asm-space name)))

(defmacro in-asm-space (name)
  `(%in-asm-space ',name))


;; clusters
(defclass asm-cluster ()
  ((base-address :accessor base-address-of :initform 0 :initarg :base-address)
   (labels :accessor labels-of :initform (make-hash-table) :initarg :labels)
   (blocks :accessor blocks-of :initform (make-hash-table) :initarg :blocks)))

(defun %def-cluster (name &key in base-address)
  (let ((asm-space (if in
                       (get-asm-space in)
                       *current-asm-space*)))
    (setf (gethash name (clusters-of asm-space))
          (make-instance 'asm-cluster
                         :base-address base-address))))

(defmacro def-cluster (name &key in (base-address 0))
  `(%def-cluster ',name :in ',in :base-address ,base-address))

(defun get-cluster (name &key in)
  (let ((asm-space (if in
                       (get-asm-space in)
                       *current-asm-space*)))
    (aif (gethash name (clusters-of asm-space))
         it
         (error "no cluster of name ~a found in asm-space ~a" name in))))

;; blocks
(defclass asm-block ()
  ((fns :accessor fns-of :initform (make-hash-table) :initarg :fns)))

(defun clear-current-block ()
  (clrhash (fns-of *current-asm-block*)))

(defun %def-block (name)
  (setf (gethash name *asm-blocks*)
        (make-instance 'asm-block)))

(defmacro def-block (name)
  `(%def-block ',name))

(defun get-block (name)
  (gethash name *asm-blocks*))

(defun get-blocks (block-list)
  (loop for block in block-list
     collect (get-block block)))

(defun %in-block (name)
  (aif (get-block name)
       (setf *current-asm-block* it)
       (error "block name ~a not found" name)))

(defmacro in-block (name)
  `(%in-block ',name))

(defun %set-block (block-name cluster-name &key in)
  (let ((cluster (get-cluster cluster-name :in in)))
    (setf (gethash block-name (blocks-of cluster))
          (aif (gethash block-name *asm-blocks*)
               it
               (error "no binding found for block named ~a" block-name)))))

(defmacro set-block (da-block cluster-name &key in)
  `(%set-block ,da-block ,cluster-name :in ',in))

(defun %set-blocks (block-list cluster-name &key in)
  (loop for block in block-list
     do (%set-block block cluster-name :in in)))

(defmacro set-blocks (block-list cluster-name &key in)
  `(%set-blocks ',block-list ',cluster-name :in ',in))

;; compound
(defmacro def-space-n-clusters-n-blocks (space-name cluster-specs block-specs)
  `(progn
     (def-asm-space ,space-name)
     ,@(loop for spec in block-specs
          collect (if (symbolp spec)
                      `(def-block ,spec)
                      `,(append '(def-block) (list (car spec)) (cdr spec) `(:in ,space-name))))
     ,@(loop for spec in cluster-specs
          collect (let* ((spec-name (car spec))
                         (spec-plist (cdr spec))
                         (block-list (getf spec-plist :blocks))
                         (un-blocked-spec-plist (remove-plist spec-plist :blocks)))
                    `(progn
                       ,(if (symbolp spec)
                            `(def-cluster ,spec :in ,space-name)
                            `,(append '(def-cluster) (list spec-name) un-blocked-spec-plist `(:in ,space-name)))
                       ,(if block-list
                            `(set-blocks ,block-list ,spec-name :in ,space-name)))))))

(def-space-n-clusters-n-blocks armish-user
    ((user-cluster :blocks (user-block)))
  (user-block))

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

(defmacro def-asm (name args &rest body)
  "fn that outputs arm code"
  `(defun ,name ,args
     (emit-asm ,@body)))

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


(defun emit-arm-fns (&optional (asm-cluster *current-asm-cluster*))
  "TODO: this is one of the major cleanup entry points into wiring up
the asm spaces stuff. the init- and final functions should be bound
to either a block, space or cluster, and emit-arm-fns should be packed up
in something more elegant"
  (append (emit-init-fn)
          (loop for asm-block being the hash-value in (blocks-of asm-cluster)
             append (loop for asm-fn being the hash-value in (fns-of asm-block)
                       append (funcall asm-fn)))
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