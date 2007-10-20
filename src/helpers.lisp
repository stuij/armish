(in-package :armish)

;; defined test suites
(def-suite arm-suite :description "test arm instructions for correctness")
(def-suite reg-suite :description "check different forms of register transformation")

;;; some register manipulater functions - a lot of code-doubling going on here, should clean it up when less lazy.
;; give register value

(in-suite reg-suite)

(defun translate-register (reg)
  (getf (list 'r0 0 'r1 1 'r2 2 'r3 3 'r4 4 'r5 5 'r6 6 'r7 7 'r8 8 'r9 9
              'r10 10 'r11 11 'r12 12 'r13 13 'sp 13 'r14 14 'lr 14 'r15 15 'pc 15)
        reg))

(defun translate-neg-register (reg)
  (getf (list '-r0 0 '-r1 1 '-r2 2 '-r3 3 '-r4 4 '-r5 5 '-r6 6 '-r7 7 '-r8 8 '-r9 9
              '-r10 10 '-r11 11 '-r12 12 '-r13 13 '-sp 13 '-r14 14 '-lr 14 '-r15 15 '-pc 15)
        reg))

(defun translate-bang-register (reg)
  (getf (list 'r0! 0 'r1! 1 'r2! 2 'r3! 3 'r4! 4 'r5! 5 'r6! 6 'r7! 7 'r8! 8 'r9! 9
              'r10! 10 'r11! 11 'r12! 12 'r13! 13 'sp! 13 'r14! 14 'lr! 14 'r15! 15 'pc! 15)
        reg))

(defun translate-register-plus-min (reg)
  (acond ((translate-register reg)
          (values it :plus))
         ((translate-neg-register reg)
          (values it :minus))))

(defun translate-register-plus-bang (reg)
  (acond ((translate-register reg)
          (values it :plus))
         ((translate-bang-register reg)
          (values it :bang))))

(defun e-translate-register (reg)
  (let ((reg-value (translate-register reg)))
    (aif reg-value
         reg-value
         (error "i don't know nothin' of no register ~a, who is this register, and where does he come from." reg))))

(defun e-translate-plus-bang-register (reg)
  (multiple-value-bind (reg-value kind)
      (translate-register-plus-bang reg)
    (if reg-value
        (values reg-value kind)
        (error "There's no register ~a, fool! Name it something else!" reg))))

;; coproc register handlers
(defun translate-coproc-register (reg)
  (getf (list 'c0 0 'c1 1 'c2 2 'c3 3 'c4 4 'c5 5 'c6 6 'c7 7 'c8 8 'c9 9
              'c10 10 'c11 11 'c12 12 'c13 13 'c14 14 'c15 15
              'cr0 0 'cr1 1 'cr2 2 'cr3 3 'cr4 4 'cr5 5 'cr6 6 'cr7 7 'cr8 8 'cr9 9
              'cr10 10 'cr11 11 'cr12 12 'cr13 13 'cr14 14 'cr15 15)
        reg))

(defun e-translate-coproc-register (reg)
  (let ((reg-value (translate-coproc-register reg)))
    (aif reg-value
         reg-value
         (error "register ~a doesn't exist in the nomenclature of this assembler" reg))))

;; handle multipe registers
(defun translate-registers (&rest reg-list)
  (values-list (loop for reg in reg-list
                  collect (translate-register reg))))

(defun e-translate-registers (&rest reg-list)
  (values-list (loop for reg in reg-list
                  collect (e-translate-register reg))))


(defun e-translate-coproc-registers (&rest reg-list)
  (values-list (loop for reg in reg-list
                  collect (e-translate-coproc-register reg))))

;; negate
(defun return-reg-antipode (reg operand)
  (let ((plus-minus '((r0 . -r0) (r1 . -r1) (r2 . -r2) (r3 . -r3) (r4 . -r4) (r5 . -r5) (r6 . -r6) (r7 . -r7) (r8 . -r8)
                      (r9 . -r9) (r10 . -r10) (r11 . -r11) (r12 . -r12) (r13 . -r13) (sp . -sp) (r14 . -r14) (lr . -lr)
                      (r15 . -r15) (pc . -pc))))
    (funcall operand reg plus-minus)))

(defun negate-reg-symbol (reg)
  (acond ((return-reg-antipode reg #'assoc) (values (cdr it) :minus))
         ((return-reg-antipode reg #'rassoc) (values (car it) :plus))
         (t (error "~a is neither a register, nor a `negative register' as they call it" reg))))

;; predicates
(defun regp (reg)
  (if (translate-register reg)
      t nil))

(defun neg-regp (reg)
  (if (translate-neg-register reg)
      t nil))

(defun bang-reg-p (reg)
  (if (translate-bang-register reg)
      t nil))

(defun plus-min-reg-p (reg)
  (if (or (translate-register reg) (translate-neg-register reg))
      t nil))

(defun any-reg-p (reg)
  (if (or (plus-min-reg-p reg)
          (translate-coproc-register reg))
      t nil))

