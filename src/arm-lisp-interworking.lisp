(in-package :armish)

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



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun escape-assembly-constants (symbol)
    (if (any-or-mr-reg-p symbol)
        `',symbol
        (reg-min-bangify symbol))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %emit-asm (instrs)
    (loop for item in instrs
       collect (etypecase item
                 (cons (append '(list) (%emit-asm item)))
                 (keyword item)
                 (symbol (escape-assembly-constants item))
                 (number item)))))

(defmacro emit-asm (&rest instrs)
  `(list
    ,@(loop for expr in instrs
         collect (etypecase expr
                   (cons (append (list 'list `',(car expr)) 
                                 (if (cdr expr) (%emit-asm (cdr expr)))))
                   (keyword expr)
                   (symbol `',expr)))))