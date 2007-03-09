(in-package :armish)

;;; some functions i tend to use:
;; nr functions
(defun positive-integer-p (nr)
  (and (integerp nr) (plusp nr)))

(defun non-neg-integer-p (nr)
  (or (positive-integer-p nr) (zerop nr)))

(defun pos-int-range (nr upper-limit)
  (and (positive-integer-p nr) (<= nr upper-limit)))

(defun non-neg-int-range (nr upper-limit)
  (and (non-neg-integer-p nr) (<= nr upper-limit)))

(defun negate (nr)
  (- 0 nr))

(defun ensure-non-negative (nr)
  (if (minusp nr)
      (negate nr)
      nr))

;; low-level helpers
(defun trans-to-base (nr base)
  "Debugging function for the non-base-ten newbie. God knows how much i used this one."
  (let ((*print-base* base))
    (format nil "~a" nr)))

(defun nr-to-big-endian-octets (nr bits)
  (let ((i (1- (/ bits 8))))
    (loop for bla from 0 to i
       collect (logand (ash nr (negate (* bla 1 8))) #xFF))))

(defun nr-to-octets (nr bits)
  (nreverse (nr-to-big-endian-octets nr bits)))

(defun nr-to-big-endian-word-byte-list (nr)
  (nr-to-big-endian-octets nr 32))

(defun nr-to-big-endian-halfword-byte-list (nr)
  (nr-to-big-endian-octets nr 16))

(defun big-endian-octets-to-nr (list bits)
  (let ((i (1- (/ bits 8))))
    (loop for bla from 0 to i
       sum (ash (nth bla list) (* bla 1 8)))))

(defun big-endian-word-byte-list-to-nr (list)
  (big-endian-octets-to-nr list 32))

(defun big-endian-halfword-byte-list-to-nr (list)
  (big-endian-octets-to-nr list 16))

(defun ror-word (integer count)
  (assert (and (<= integer #xffffffff) (<= count 32)))
  (let ((ashable (ash integer (-  0 count))))
    (+ ashable
       (ash (logand integer
                    (- #xffffffff (logand #xffffffff (ash #xffffffff count))))
            (- 32 count)))))

(defun rol-word (integer count)
  (assert (and (<= integer #xffffffff) (<= count 32)))
  (let ((ashed (ash integer count)))
    (+ (logand ashed
               #xffffffff)
       (ash (logand #xffffffff00000000 ashed) -32))))

(defun encode-twos-complement (nr bits)
  (let ((bit-max (1- (ash 1 bits))))
    (if (minusp nr)
        (let ((negated (negate nr)))
          (assert (<= negated bit-max))
          (logand (+ 1 (lognot negated))
                  bit-max))
        (progn
          (assert (<= nr bit-max))
          nr))))

(defun aligned (address &optional bytes)
  "Returns the next bytes byte aligned address. Defaults to 4."
  (let ((offset (mod address (if bytes bytes 4))))
    (if (zerop offset)
        address
        (+ (- (if bytes bytes 4) offset) address))))

(defun align (byte-lst &optional bytes)
  "Aligns a list of bytes by padding zeros to a byte byte boundry. Defaults to 4."
  (let ((offset (mod (length byte-lst) (if bytes bytes 4))))
    (if (zerop offset)
        byte-lst
        (append byte-lst (make-list (- (if bytes bytes 4) offset) :initial-element 0)))))

(defun process-output (process)
  #+allegro (third process)
  #+cmu (ext:process-output process)
  #+ecl process
  #+gcl (si::fp-input-stream process)
  #+sbcl (sb-ext:process-output process)
  #+lispworks (sys::open-pipe process)
  #+clisp (#+lisp=cl ext:make-pipe-input-stream
                     #-lisp=cl lisp:make-pipe-input-stream process)
  #-(or allegro cmu ecl gcl sbcl lispworks clisp)
  (error "Sorry, the function `process-output' is not supported for your implementation.
          Look in helpers.lisp and add the process output function for your implementation if you know how.
          Else mail the maintainers. Thanks!"))

;; other
(defun vector-to-list (sequence)
  "they don't come much more simpler"
  (loop for s across sequence collect s))

;; from norvig
(defun concat-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

;; from clocc
(defun remove-plist (plist &rest keys)
  "Remove the keys from the plist.
   Useful for re-using the &REST arg after removing some options."
  (do (copy rest)
      ((null (setq rest (nth-value 2 (get-properties plist keys))))
       (nreconc copy plist))
    (do () ((eq plist rest))
      (push (pop plist) copy)
      (push (pop plist) copy))
    (setq plist (cddr plist))))

(defun run-prog (prog &rest opts &key args (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  #+gcl (declare (ignore wait))
  (setq opts (remove-plist opts :args :wait))
  #+allegro (apply #'excl:run-shell-command (apply #'vector prog prog args)
                   :wait wait opts)
  #+(and clisp      lisp=cl)
  (apply #'ext:run-program prog :arguments args :wait wait opts)
  #+(and clisp (not lisp=cl))
  (if wait
      (apply #'lisp:run-program prog :arguments args opts)
      (lisp:shell (format nil "~a~{ '~a'~} &" prog args)))
  #+cmu (apply #'ext:run-program prog args :wait wait opts)
  #+gcl (apply #'si:run-process prog args)
  #+liquid (apply #'lcl:run-program prog args)
  #+lispworks (apply #'sys::call-system
                     (format nil "~a~{ '~a'~}~@[ &~]" prog args (not wait))
                     opts)
  #+lucid (apply #'lcl:run-program prog :wait wait :arguments args opts)
  #+sbcl (apply #'sb-ext:run-program prog args :wait wait opts)
  #-(or allegro clisp cmu gcl liquid lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'run-prog prog opts)))