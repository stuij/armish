(in-package :armish)

(defmacro define-directive (name args &body body)
  "Makes a new instruction function and adds it to the instruction set."
  `(setf (gethash ',name *directives*) (lambda ,args ,@body)))

(defmacro define-se-directive (name args &body body)
  "define a directive which is meant just for side-effects. Will always return nil"
  `(define-directive ,name ,args
     ,@body
     nil))

(defun process-bytes (bytes bit-size)
  (assert bytes)
  (let ((8-bit-bytes (loop for nr in bytes
                        nconc (reverse (nr-to-octets nr bit-size)))))
    (loop while (not (null 8-bit-bytes))
       nconc (remove nil (loop for i from 1 to 4
                            collect (pop 8-bit-bytes))))))

(define-directive dcb (&rest bytes)
  (process-bytes bytes 8))

(define-directive byte (&rest bytes)
  (process-bytes bytes 8))

(define-directive dcw (&rest bytes)
  (process-bytes bytes 16))

(define-directive hword (&rest bytes)
  (process-bytes bytes 16))

(define-directive dcd (&rest bytes)
  (process-bytes bytes 32))

(define-directive word (&rest bytes)
  (process-bytes bytes 32))

(define-directive dword (&rest bytes)
  (process-bytes bytes 64))

(define-directive quad (&rest bytes)
  (process-bytes bytes 64))

(define-directive bin (pack-size bin)
  (process-bytes bin (* pack-size 8)))

(define-directive binae (bins)
  (loop for bin in bins
     append (process-bytes (cadr bin) (* (car bin) 8))))

(defun reform-string (string)
  (process-bytes (vector-to-list
                  (string-to-octets string *string-encoding*)) 8))

(define-directive string (&rest strings)
  (let ((null-terminated (or (member :null-terminated strings)
                             (member :nt strings)))
        (strings-string (apply #'concatenate
                               'string
                               (delete :nt (delete :null-terminated strings)))))
    (if null-terminated
        (append (reform-string strings-string) '(*string-end*))
        (reform-string strings-string))))

(define-directive space (size &optional (fill 0))
  (make-list size :initial-element fill))

(define-directive align (&optional bytes)
  (align-assembled bytes))

(define-se-directive def-asm-param (name init-val)
  (if (gethash name *asm-params*)
      (error "asm parameter ~a has already been defined" name)
      (setf (gethash name *asm-params*) init-val)))

(define-se-directive set-asm-param (name new-val)
  (if (gethash name *asm-params*)
      (setf (gethash name *asm-params*) new-val)
      (error "asm parameter ~a hasn't been defined" name)))

(defun call-directive (name &rest params)
  (apply (gethash name *directives*) params))

(defun get-hw-label-address (label)
  (ecase *pass*
    (0 -1)
    (1 (+ *base-address* (label-address label)))))

(defun get-asm-param (name)
  (gethash name *asm-params*))

(defun asm-param-p (name)
  (get-asm-param name))

(defun directive-form-p (form)
  (gethash (intern (symbol-name (car form)) :armish) *directives*))

;; directive symbols
(defmacro add-directive-symbol (name &body forms)
  `(setf (gethash ',name *directive-symbols*)
         (lambda ()
           ,@forms)))

(defun directive-symbol-p (symbol)
  (gethash symbol *directive-symbols*))

(defun exec-directive-symbol (symbol)
  (funcall (gethash symbol *directive-symbols*)))

(add-directive-symbol code32
  (set-mode *arm*)
  (align-assembled))

(add-directive-symbol code16
  (set-mode *thumb*)
  (align-assembled 2))

(add-directive-symbol align
  (align-assembled))

(add-directive-symbol align-hw
  (align-assembled 2))

(add-directive-symbol pool
  (dump-pool))