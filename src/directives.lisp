(in-package :armish)

(defmacro define-directive (name args &body body)
  "Makes a new instruction function and adds it to the instruction set."
  `(setf (gethash ',name *directives*) (lambda ,args ,@body)))

(defun process-bytes (bytes bit-size)
  (assert bytes)
  (let ((8-bit-bytes (if (= bit-size 8)
                         bytes
                         (loop for nr in bytes
                            nconc (reverse (nr-to-octets nr bit-size))))))
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

(define-directive binae (pack-size bins)
  (loop for bin in bins
     append (process-bytes bin (* pack-size 8))))

(defun reform-string (string)
  (process-bytes (vector-to-list
                  (string-to-octets string :utf-8)) 32))

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
