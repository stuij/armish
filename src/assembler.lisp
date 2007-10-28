(in-package :armish)

(defvar *arm* (make-hash-table)
  "Arm instruction set")
(defvar *thumb* (make-hash-table) 
  "Thumb instruction set")
(defvar *directives* (make-hash-table)
  "assembler directives")

(defvar *pass*)                         ; current pass (0 or 1)
(defvar *here*)                         ; offset address
(defvar *size*)                         ; final size of pass
(defvar *mode*)                         ; arm or thumb
(defvar *labels*)                       ; local assembly labels
(defvar *version*)                     ; processor capabilities and enhancements
(defvar *pool*)                         ; current active literal pool
(defvar *pool-position*)                ; position of the current pool
(defvar *pool-pairs*) ; list of cons who's car is a literary pool and who's cdr is
                                        ; the end address of it's offset

;; setters, getters and checkers
(defun check-version (lower-limit)
  (let ((limit-val (get-version lower-limit)))
    (if (< *version* limit-val)
        (error "instruction + argument combination is not an option on this processor type")
        *version*)))

(defun get-version (proc-spec)
  (getf (list 0 0  'all 0
              3 3
              4 4 'version-4 4 '4t 4.2 'ARM7TDMI 4.2 'arm7 4.2
              5 5 'version-5 5 '5TExP 5.3 '5TE 5.4 'ARM946E-S 5.4 'arm9 5.4)
        proc-spec))

(defun set-version (proc-spec)
  (setf *version* (get-version proc-spec)))

(defun get-mode (mode)
  (getf (list 'arm   *arm*   *arm*   *arm*   'code32 *arm*
              'thumb *thumb* *thumb* *thumb* 'code16 *thumb*)
        mode))

(defun set-mode (mode)
  (setf *mode* (get-mode mode)))

(defun check-m ()
  (getf (list 3 nil 3.1 t
              4 t 4.1 nil 4.2 t
              5 t 5.1 nil 5.2 t 5.3 t 5.4 t)
        *version*))

(defun check-feature (feature)
  (case feature
    (M (check-m))
    (otherwise (error "feature ~a not supported" feature))))

;; the meat of the assembler; it's a bit skinny if you ask me

(defun align-assembled (&optional bytes)
  (let ((aligned (aligned *here* bytes))
        (here-tmp *here*))
    (if (not (= aligned *here*))
        (progn (setf *here* aligned)
               (make-list (- aligned here-tmp) :initial-element 0)))))

(defun pool ()
  "Returns a list of bytes representing all the literals in the pool."
  (loop for literal in *pool* nconc
       (nr-to-big-endian-word-byte-list literal)))

(defun bind-next-pool ()
  (let ((cons (pop *pool-pairs*)))
    (setf (values *pool* *pool-position*) (values (car cons) (cdr cons)))))

(defun pool-position ()
  *pool-position*)

(defun dump-pool ()
  (let ((there *here*))
    (incf *here* (* 4 (length *pool*)))
    (if (= *pass* 0)
        (progn (push (cons *pool* there) *pool-pairs*)
               (setf *pool* '())
               0) 
        (let ((this-pool (pool)))
          (bind-next-pool)
          this-pool))))

(defun label-address (symbol)
  "Return the address of a symbol."
  (if (zerop *pass*)
      0
      (gethash symbol *labels*)))

(defun resolve-symbol (symbol)
  (case symbol
    (code32 (progn (set-mode *arm*)
                   (align-assembled)))
    (code16 (progn (set-mode *thumb*)
                   (align-assembled 2)))
    (align    (align-assembled))
    (align-hw (align-assembled 2))
    (pool     (dump-pool))
    (otherwise (if (= *pass* 0)
                   (setf (gethash symbol *labels*) *here*)))))

(defun assemble-form (form)
  "Looks up an instruction in the instruction set and assembles with arguments."
  (aif (gethash (first form) *mode*)
       (mapcan #'(lambda (opcode)
                   (when opcode
                     (cond ((eq *mode* *arm*) (nr-to-big-endian-word-byte-list opcode))
                           ((eq *mode* *thumb*) (nr-to-big-endian-halfword-byte-list opcode)))))
               (ensure-list (apply it (rest form))))
       (aif (gethash (first form) *directives*)
            (apply it (rest form))
            (error "mnemonic ~A from form ~A not recognized" (first form) form))))

(defun reform-string (string)
  (vector-to-list (string-to-octets string :utf-8)))

(defun pass-1 (forms)
  "First pass assembler to collect all label addresses."
  (let ((*pass* 0)
        (*here* 0))
    (dolist (form forms *here*)
      (cond ((symbolp form)
             (resolve-symbol form))
            ((listp form)
             (incf *here* (length (assemble-form form))))
            ((stringp form)
             (incf *here* (length (append (reform-string form) '(0)))))))
    (dump-pool)))

(defun pass-2 (forms)
  "Second pass assembler that actually returns opcodes."
  (let ((*pass* 1)
        (*here* 0))
    (loop for form in forms append
         (cond
           ((symbolp form)
            (resolve-symbol form))
           ((listp form)
            (let ((opcode (assemble-form form)))
              (incf *here* (length opcode))
              opcode))
           ((stringp form)
            (let ((string-code (append (reform-string form) '(0))))
              (incf *here* (length string-code))
              string-code))))))

(defun clean-form (form)
  (cond
    ((keywordp form) form)
    ((symbolp form) (intern (symbol-name form) 'armish))
    ((stringp form) form)
    ((numberp form) form)
    ((listp form) 
     (loop for thing in form
        collect (clean-form thing)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %assemble (forms &key washed chip mode)
    "Two pass assembler for all given forms and labels."
    (let* ( ;; internalize symbols so we can compare them
           (pure-forms (if washed
                           forms
                           (clean-form (append forms '(align pool :code-end)))))
           ;; setup initial specials
           
           (*labels* (make-hash-table))
           (*pool* '())
           (*pool-position* 0)
           (*pool-pairs* '())
           (*version* (if chip (get-version (intern (symbol-name chip) 'armish)) *version*))
           (*mode* (if mode (get-mode (intern (symbol-name mode) 'armish)) *mode*))
           (mode-tmp *mode*)
           ;; all set up! now we go
           (*size* (aligned (pass-1 pure-forms)))
           (*mode* mode-tmp)
           (*pool-pairs* (nreverse *pool-pairs*)))
      (bind-next-pool)
      (align (pass-2 pure-forms)))))

(defun assemble (chip mode forms)
  (%assemble forms :chip chip :mode mode))