(in-package :armish)

;;{{{ general functions/macro's/parameters

(defmacro define-arm-instruction (name args &body body)
  "Makes a new arm instruction function and adds it to the instruction set."
  `(setf (gethash ',name *arm*) (lambda ,args ,@body)))

(defun cut-symbol-appendix (name appendix)
  (let ((name-string (format nil "~a" name)))
    (intern (subseq name-string 0 (- (length name-string) (length (format nil "~a" appendix)))))))

(defun make-and-install-condition-fns (instr-name &key suffix)
  (let ((un-suffixed-name (if suffix
                              (cut-symbol-appendix instr-name suffix)
                              instr-name)))
    (flet ((attach-condition (fn cond-value)
             (if (eql instr-name 'blx)
                 (lambda (&rest args)
                   (assert (translate-register (car args)))
                   (+ (apply fn args)
                      (ash cond-value 28)))
                 (lambda (&rest args)
                   (+ (apply fn args)
                      (ash cond-value 28))))))
      (let ((conds  '((EQ . #b0000) (NE . #b0001) (CS . #b0010) (HS . #b0010) (CC . #b0011) (LO . #b0011)
                      (MI . #b0100) (PL . #b0101) (VS . #b0110) (VC . #b0111) (HI . #b1000) (LS . #b1001)
                      (GE . #b1010) (LT . #b1011) (GT . #b1100) (LE . #b1101) (AL . #b1110) (NV . #b1111)))
            (instr-fn (gethash instr-name *arm*)))
        (if (not (eql instr-name 'blx)) (setf (gethash instr-name *arm*) (attach-condition instr-fn #b1110)))
        (loop for (cond . cond-val) in conds
           do (setf (gethash (intern (if suffix
                                         (format nil "~a~a~a" un-suffixed-name cond suffix)
                                         (format nil "~a~a" instr-name cond)))
                             *arm*)
                    (attach-condition (if (eql cond 'nv) (lambda (&rest args)
                                                           (declare (ignorable args))
                                                           (error "Using nv in ver. 2 and below makes your instr. do nothing, 
In ver. 3 and 4 the behaviour is undefined and in ver. 5 (and above?) totally filling up the condition fields in this way is not allowed,
unless individual instructions do allow it, in which case you should report an error if you would be so kind.
So sorry, but i'm just gonna error you on this outrageous misuse of nv."))
                                          instr-fn) cond-val)))))))

(defun enhance-instr-fn (instr-name appendix value)
  (let ((instr-fn (gethash instr-name *arm*))
        (enhanced-instr-name (concat-symbol instr-name appendix)))
    (setf (gethash enhanced-instr-name *arm*)
          (lambda (&rest body)
            (+ (apply instr-fn body)
               value)))
    enhanced-instr-name))

(defun do-enhance-plus-conditions (name suffix value)
  (make-and-install-condition-fns (enhance-instr-fn name suffix value) :suffix suffix))

(defun do-conds-and-enhance-plus-conds (name suffix value)
  (do-enhance-plus-conditions name suffix value)
  (make-and-install-condition-fns name))

(defun process-l-s-index-reg (reg)
  (multiple-value-bind (reg-val sign)
      (translate-register-plus-min reg)
    (+ (if (eql sign :plus) (ash 1 23) 0)
       reg-val)))

(defun process-shiftable-immediate (nr)
  (assert (<= nr #xffffffff))
  (if (<= nr #xFF)
      (values nr 0)
      (loop
         with imm = 0
         for shift from #x1 to #xF
         do (setf imm (rol-word nr (* shift 2)))
         if (<= imm #xFF)
         return (values imm shift)
         finally (error "Immediate #x~x is not encodable by ror-ing a 256-max value by a 2-divisable shift in a 32 bit word.~&Blame the maths laws (or the arm-bytecode-specification-inventor-guy (or the arm-code optimizer (if it's written by that time~&(and if you're using it of course) or yourself))" nr))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun creator-helper (creator-name instr-alist)
    (loop for (instr-name . code) in instr-alist
       collect `(,creator-name ,instr-name ,code))))

(defmacro creator-loop (creator-name instr-alist)
  `(progn ,@(creator-helper creator-name instr-alist)))

;;; some register manipulater functions - a lot of code-doubling going on here, should clean it up when less lazy.
;; give register value
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

(defun plus-min-reg-p (reg)
  (if (or (translate-register reg) (translate-neg-register reg))
      t nil))

;;}}}
;;{{{ instructions

;;{{{ instructions: addressing mode 1 - data processing operands

(defun dp-immediate (imm)
  (multiple-value-bind (base_imm rot_imm)
      (process-shiftable-immediate imm)
    (+ (ash 1 25)
       (ash rot_imm 8)
       base_imm)))

(defun dp-shifter-immediate-check (val)
  (if (<= val #b11111) val (error "dp-instr shiftable imm is bigger than 31")))

(defun process-dp-register-shifter (shifter shiftee)
  (+ (ash (translate-register shiftee) 8)
     (case shifter
       (:lsl (ash 1 4))
       (:lsr (ash 3 4))
       (:asr (ash 5 4))
       (:ror (ash 8 4))
       (otherwise (error "shifter ~a not eql to :lsl :lsr :asr or :ror" shifter)))))

(defun process-dp-immediate-shifter (shifter shiftee)
  (+ (ash (dp-shifter-immediate-check shiftee) 7)
     (case shifter
       (:lsl 0)
       (:lsr (ash 1 5))
       (:asr (ash 2 5))
       (:ror (ash 3 5))
       (otherwise (error "shifter ~a not eql to :lsl :lsr :asr or :ror" shifter)))))

(defun process-dp-shifter-pair (shifter shiftable)
  (cond ((eql shifter :rrx)
         (if shiftable
             (error "Shiftable value supplied with rrx. That makes no sense.")
             (ash 3 5)))
        ((regp shiftable) (process-dp-register-shifter shifter shiftable))
        (t (process-dp-immediate-shifter shifter shiftable))))

(defun process-shifter-operand (rm shifter-operand to-be-shifted)
  (if (integerp rm)
      (dp-immediate rm)
      (+ (e-translate-register rm)
         (cond
           ((keywordp shifter-operand) (process-dp-shifter-pair shifter-operand to-be-shifted))
           ((and (null shifter-operand) (null to-be-shifted)) 0)
           ((symbolp shifter-operand) (e-translate-register shifter-operand))
           (t (error "summ.'s wrong with your arguments: ~a and ~a if applicable" shifter-operand to-be-shifted))))))

(defmacro create-generic-dp-instruction (name opcode)
  `(progn
     (define-arm-instruction ,name (rd rn &optional rm shifter-operand to-be-shifted)
       (multiple-value-bind (rn rd)
           (e-translate-registers rn rd)
         (+ (ash ,opcode 21) (ash rn 16) (ash rd 12)
            (process-shifter-operand rm shifter-operand to-be-shifted))))
     (do-conds-and-enhance-plus-conds ',name 's (ash #b1 20))))

(defmacro create-moving-dp-instruction (name opcode)
  "same as create-generic-dp-instruction, but rn is gone"
  `(progn
     (define-arm-instruction ,name (rd rm &optional shifter-operand to-be-shifted)
       (let ((rd (e-translate-register rd)))
         (+ (ash ,opcode 21)
            (ash rd 12)
            (process-shifter-operand rm shifter-operand to-be-shifted))))
     (do-conds-and-enhance-plus-conds ',name 's (ash #b1 20))))

(defmacro create-comparing-dp-instruction (name opcode)
  "same as create-generic-dp-instruction, but s is gone (bit 20 is always 1) and there is no rd to be encoded"
  `(progn
     (define-arm-instruction ,name (rn rm &optional shifter-operand to-be-shifted)
       (let ((rn (e-translate-register rn)))
         (+ (ash ,opcode 21) (ash rn 16) (ash 1 20)
            (process-shifter-operand rm shifter-operand to-be-shifted))))
     (make-and-install-condition-fns ',name)))

(creator-loop create-generic-dp-instruction ((adc . #b0101) (add . #b0100) (and . #b0000) (bic . #b1110) (eor . #b0001)
                                             (orr . #b1100) (rsb . #b0011) (rsc . #b0111) (sbc . #b0110) (sub . #b0010)))

(creator-loop create-moving-dp-instruction ((mov . #b1101) (mvn . #b1111)))
(creator-loop create-comparing-dp-instruction ((cmn . #b1011) (cmp . #b1010) (teq . #b1001) (tst . #b1000)))

;;}}}
;;{{{ instructions: addressing mode 2 - load and store word or unsigned byte

(defun process-arbitrary-l-s-shiftee (shiftee)
  (assert (and (positive-integer-p shiftee) (<= shiftee 32)))
  (if (= shiftee 32) 0 (ash shiftee 7)))

(defun process-l-s-shifter-pair (shifter shiftee)
  (case shifter
    (:lsl
     (assert (and (non-neg-integer-p shiftee) (<= shiftee 31)))
     (ash shiftee 7))
    (:lsr
     (+ (ash 1 5)
        (process-arbitrary-l-s-shiftee shiftee)))
    (:asr
     (+ (ash 2 5)
        (process-arbitrary-l-s-shiftee shiftee)))
    (:ror
     (assert (and (positive-integer-p shiftee) (<= shiftee 31)))
     (+ (ash 3 5)
        (ash shiftee 7)))
    (:rrx
     (assert (null shiftee))
     (ash 3 5))))

(defun l-s-offset (index shifter shiftee)
  (cond ((integerp index)
         (assert (and (null shifter) (null shiftee)) (shifter shiftee)
                 "immediate offset with superfluous arguments ~a and ~a. should both be nil" shifter shiftee)
         (assert (and (<= index 4095) (>= index -4095)) (index) "immediate is larger than 4095 or smaller than -4095")
         (if (plusp index)
             (+ index (ash 1 23))
             (negate index)))
        ((keywordp shifter)
         (assert (plus-min-reg-p index))
         (+ (ash 1 25)
            (process-l-s-shifter-pair shifter shiftee)
            (process-l-s-index-reg index)))
        ((plus-min-reg-p index)
         (assert (and (null shifter) (null shiftee)) (shifter shiftee)
                 "register offset with extra arguments ~a and ~a. should both be nil" shifter shiftee)
         (+ (ash 1 25)
            (process-l-s-index-reg index)))))

(defmacro create-l-s-w-b (name &key byte-bit translation load suffix ldr)
  `(progn
     (define-arm-instruction ,name (rd rn-list &optional index/update shifter shiftee)
       (multiple-value-bind (rd rn)
           (e-translate-registers rd ,(if ldr
                                          '(if (integerp rn-list) 'r15 (car rn-list)) ;; ugliness to handle ldr constant loading 
                                          '(car rn-list)))
         (+ (ash 1 26) (ash rd 12) (ash rn 16)
            ,(if load '(ash 1 20) 0) ;; otherwise store
            ,(if translation '(ash 1 21) 0)
            ,(if byte-bit '(ash 1 22) 0)
            (cond (,@( ;; a very crude load constant implementation.
                      ;; TODO: encode the constant more efficiently if possible in stead of always loading from memory. 
                      if ldr '((integerp rn-list)
                               (assert (non-neg-int-range rn-list #xFFFFFFFF))
                               (let ((offset 0))
                                 (if (zerop *pass*)
                                     (unless (member rn-list *pool*)
                                       (push rn-list *pool*)) ; add literal to pool
                                     (incf offset (+ (- (pool-position) (+ *here* 8))
                                                     (ash (position rn-list *pool*) 2)))) ; find pc offset to literal
                                 (+ (ash 1 24) (l-s-offset offset shifter shiftee))))
                      '(nil)))
                  ;; pre-indexed
                  ((eql index/update '!)
                   ,(if translation
                        '(error "you can only use postindexing with translation load/store instructions, so no pre-indexing")
                        '(+ (l-s-offset (cadr rn-list) (caddr rn-list) (cadddr rn-list))
                          (ash 1 24) (ash 1 21))))
                  ;; post-indexed
                  (index/update
                   (l-s-offset index/update shifter shiftee))
                  ;; offset indexing
                  (t                   
                   ,(if translation
                        '(error "you can only use postindexing with translation load/store instructions, so no offset indexing")
                        '(+ (if (cadr rn-list) (l-s-offset (cadr rn-list) (caddr rn-list) (cadddr rn-list)) 0)
                          (ash 1 24))))))))
     ,(if suffix
          `(make-and-install-condition-fns ',name :suffix ',suffix)
          `(make-and-install-condition-fns ',name))))

(create-l-s-w-b ldr :load t :ldr t)
(create-l-s-w-b ldrb :suffix b :byte-bit t :load t)
(create-l-s-w-b ldrbt :suffix bt :byte-bit t :translation t :load t)
(create-l-s-w-b ldrt :suffix t :translation t :load t)

(create-l-s-w-b str)
(create-l-s-w-b strb :suffix b :byte-bit t)
(create-l-s-w-b strbt :suffix bt :byte-bit t :translation t)
(create-l-s-w-b strt :suffix t :translation t)

;;}}}
;;{{{ instructions: addressing mode 3 - miscellaneous loads and stores

(defun misc-l-s-offset (offset)
  (cond ((integerp offset)
         (assert (and (<= offset #xFF) (>= offset #x-FF)) (offset) "immediate is larger than 255 or smaller than -255")       
         (+ (if (non-neg-integer-p offset)
                (ash 1 23)
                (progn (setf offset (negate offset))
                       0))
            (ash 1 22)
            (logand offset #xF)
            (ash (ash offset -4) 8)))
        ((plus-min-reg-p offset)
         (process-l-s-index-reg offset))
        (t (error "offset value ~a was not recognized as either an integer or a valid register" offset))))

(defmacro create-misc-l-s (name &key suffix l s h dsp)
  `(progn
     (define-arm-instruction ,name (rd rn-list &optional offset)
       ,(if dsp '(check-version '5TE))
       ,(if (or s h) '(check-version 4))
       (multiple-value-bind (rd rn)
           (e-translate-registers rd (car rn-list))
         ,(if dsp '(assert (and (evenp rd) (not (= 14 rd)))))
         (+ (ash rd 12) (ash rn 16) (ash 1 7) (ash 1 4)
            ,(if l '(ash 1 20) 0)   ;; otherwise store
            ,(if s '(ash 1 6) 0)    ;; otherwise unsigned 
            ,(if h '(ash 1 5) 0)    ;; otherwise byte
            (cond ((eql offset '!)  ;; pre-indexed
                   (+ (misc-l-s-offset (cadr rn-list))
                      (ash 1 24) (ash 1 21)))
                  (offset ;; post-indexed
                   (misc-l-s-offset offset))
                  (t
                   (+ (misc-l-s-offset (cadr rn-list))
                      (ash 1 24)))))))
     (make-and-install-condition-fns ',name :suffix ',suffix)))

(create-misc-l-s ldrh :suffix h :l t :h t)
(create-misc-l-s ldrsh :suffix sh :l t :s t :h t)
(create-misc-l-s ldrsb :suffix sb :l t :s t)
(create-misc-l-s ldrd :suffix d :s t :dsp t)

(create-misc-l-s strh :suffix h :h t)
(create-misc-l-s strd :suffix d :s t :h t :dsp t)

;;}}}
;;{{{ instructions: addressing mode 4 - load and store multiple

(defun reg-to-bit (reg-val)
  (ash 1 reg-val))

(defun enum-regs (regs)
  (let ((sorted-reg-vals (sort (multiple-value-list (e-translate-registers (intern (first regs)) (intern (second regs)))) #'<)))
    (loop for i from (car sorted-reg-vals) upto (cadr sorted-reg-vals)
       collect (reg-to-bit i))))

(defun expand-reg-token (reg-token)
  (let ((regs (split-sequence #\- (format nil "~a" reg-token))))
    (if (> (length regs) 1)
        (enum-regs regs)
        (reg-to-bit (e-translate-register (intern (car regs)))))))

(defun gather-reglist-values (reglist)
  (let ((expanded-car (ensure-list (expand-reg-token (car reglist)))))
    (if (not (cdr reglist))
        expanded-car
        (let ((expanded-cdr (gather-reglist-values (cdr reglist))))
          (if (intersection expanded-car expanded-cdr)
              (error "register list in load/store multiple contains duplicate registers")
              (append expanded-car expanded-cdr))))))

(defun figure-out-register-list (register-list type loadp caretp)
  (let* ((register-value-list (gather-reglist-values register-list))
         (pcp (member (ash 1 15) register-value-list))
         (total-register-value (apply #'+ register-value-list)))
    (+ (if (eql type :bang)
           (if caretp
               (if loadp
                   (progn (assert pcp (pcp) "pc should be present in the register list for a load multiple if you want to mix a writeback bang with carets")
                          (ash 1 21))
                   (error "store multiples can't mix writeback bangs with carets"))
               (ash 1 21))
           0)
       total-register-value)))

(defmacro create-l-s-multiple (name suffix value &key load)
  `(progn
     (define-arm-instruction ,(concat-symbol name suffix) (rn register-list &optional caret)
       (multiple-value-bind (rn type)
           (e-translate-plus-bang-register rn)
         (assert (not (= rn 15)) (rn) "Specifying the pc as base register has unpredictible results.
Of course i realize you are a wizard coder and you WANT these results, but i just thought i'd signal a recoverable error anyway.")
         (+ (ash rn 16) (ash 1 27)
            ,value
            ,(if load '(ash 1 20) 0)
            (if caret
                (progn
                  (assert (eql caret '^) (caret) "the fourth argument to a load/store multiple instruction should be a `^' 
and ONLY a `^', so not a(n) ~a")
                  (ash 1 22))
                0)
            (figure-out-register-list register-list type ,load caret))))
     (make-and-install-condition-fns ',(concat-symbol name suffix) :suffix ',suffix)))

(defmacro iterate-l-s-multiple-addr-modes (name mode-list &key load)
  `(progn ,@(loop for (mode1 mode2 value) in mode-list
               collect `(progn (create-l-s-multiple ,name ,mode1 ,value :load ,load)
                               (create-l-s-multiple ,name ,mode2 ,value :load ,load)))))

(iterate-l-s-multiple-addr-modes ldm ((ia fd #x800000) (ib ed #x1800000) (da fa 0) (db ea #x1000000))
                                 :load t)

(iterate-l-s-multiple-addr-modes stm ((ia ea #x800000) (ib fa #x1800000) (da ed 0) (db fd #x1000000)))

;;}}}
;;{{{ instructions: addressing mode 5 - load and store coprocessor

(defun translate-coproc (coproc)
  "Translate coproc value into a value that fits in the appropriate bits in the load/store coproc opcode. 
   Arm-Arm states that the p1-p15 values are only generic values which could just as well be other names.
   Need to abstract perhaps a bit to account for this possibility, but this is a strong contender for scratch an itch methinks."
  (getf (list 'p0 0 'p1 1 'p2 2 'p3 3 'p4 4 'p5 5 'p6 6 'p7 7 'p8 8 'p9 9
              'p10 10 'p11 11 'p12 12 'p13 13 'p14 14 'p15 15
              'cp0 0 'cp1 1 'cp2 2 'cp3 3 'cp4 4 'cp5 5 'cp6 6 'cp7 7 'cp8 8 'cp9 9
              'cp10 10 'cp11 11 'cp12 12 'cp13 13 'cp14 14 'cp15 15)
        coproc))

(defun e-translate-coproc (coproc)
  (let ((coproc-value (translate-coproc coproc)))
    (if coproc-value
        coproc-value
        (error "Coproc ~a doesn't ring a bell with me. Never heard of that coproc. Sorry." coproc))))

(defun handle-coproc-offset (offset)
  (let ((nn-shrinkfit-offset (ash (ensure-non-negative offset) -2)))
    (+ (if (non-neg-integer-p offset)
           (ash 1 23)
           0)
       (if (and (= (mod offset 4)) (<= nn-shrinkfit-offset #xFF) )
           nn-shrinkfit-offset
           (error "coproc offset ~a either bigger than #xFF or is not a multiple of 4" offset)))))

(defmacro create-l-s-coprocessor (name &key load two)
  `(progn
     (define-arm-instruction ,name (coproc crd rn-list &optional quicksilver)
       ,(if two '(check-version 5))
       (let ((coproc (e-translate-coproc coproc))
             (crd (e-translate-coproc-register crd))
             (rn (e-translate-register (car rn-list))))
         (+ ,(if two '(ash #xF 28) 0)
            (ash coproc 8) (ash crd 12) (ash rn 16) (ash 3 26)
            ,(if load (ash 1 20) 0)
            (cond ((eql quicksilver '!)
                   (assert (not (caddr rn-list)))
                   (+ (ash 1 21) (ash 1 24)
                      (handle-coproc-offset (cadr rn-list)))) 
                  ((eql quicksilver nil)
                   (assert (not (caddr rn-list)))
                   (+ (ash 1 24)
                      (handle-coproc-offset (cadr rn-list))))
                  ((integerp quicksilver)
                   (assert (not (cadr rn-list)))
                   (+ (ash 1 21)
                      (handle-coproc-offset quicksilver)))
                  ((listp quicksilver)
                   (let ((quick-val (car quicksilver)))
                     (if (and (non-neg-integer-p quick-val) (<= quick-val #xFF))
                         (+ quick-val
                            (ash 1 23))
                         (error "instruction option ~a is negative, bigger than 255 or it is evil in a class all by itself"
                                quicksilver))))
                  (t (error "coprocessor instruction with arguments coproc: ~a crd: ~a rn-list: ~a and multi-interprettable ~a is faulty"
                            coproc crd rn-list quicksilver))))))
     ,(if two
          `(enhance-instr-fn ',name 'l (ash #b1 22))
          `(do-conds-and-enhance-plus-conds ',name 'l (ash #b1 22)))))

(create-l-s-coprocessor ldc :load t)
(create-l-s-coprocessor ldc2 :load t :two t)
(create-l-s-coprocessor stc)
(create-l-s-coprocessor stc2 :two t)

;;}}}
;;{{{ instructions: miscellaneous

;;{{{ instructions: miscellaneous: branch instructions

(progn
  (define-arm-instruction b (label)
    (let ((offset (- (label-address label) (+ *here* 8))))
      (assert (zerop (logand offset 3))) ; 4-byte aligned offset
      (+ (ash #b101 25)
         (encode-twos-complement (ash offset -2) 24))))
  (make-and-install-condition-fns (enhance-instr-fn 'b 'l (ash #b1 24)))
  (make-and-install-condition-fns 'b))

(progn
  (define-arm-instruction blx (target)
    (check-version 5)
    (aif (translate-register target)
         (progn
           (assert (not (= it 15)))
           (+ #x12FFF30
              it))
         (let ((offset (- (label-address target) (+ *here* 8))))
           (assert (zerop (logand offset 3))) ; 4-byte aligned offset
           (+ (ash #b1111101 25)
              (logand (ash offset -2) #xffffff)
              (ash (logand offset #b10) 23)))))
  (make-and-install-condition-fns 'blx))

(progn
  (define-arm-instruction bx (rm)
    (check-version '4t)
    (let ((rm (e-translate-register rm)))
      (+ #x12FFF10 rm)))
  (make-and-install-condition-fns 'bx))

;;}}}
;;{{{ instructions: miscellaneous: coprocessor instructions

(defmacro create-cdp (name &key two)
  `(progn
     (define-arm-instruction ,name (coproc opcode-1 crd crn crm opcode-2)
       ,(if two '(check-version 5))
       (multiple-value-bind (crd crn crm)
           (e-translate-coproc-registers crd crn crm)
         (assert (and (<= coproc #xF) (<= opcode-1 #xF) (<= opcode-2 #x8)))
         (+ ,(if two '(ash #b1111 28) 0)
            (ash #b1110 24) (ash coproc 8) (ash opcode-1 20) (ash crd 12) (ash crn 16) crm (ash opcode-2 5))))
     ,(if (not two) '(make-and-install-condition-fns 'cdp))))

(create-cdp cdp)
(create-cdp cdp2 :two t)

(defmacro create-move-coprocessor (name &key bit-20 two)
  `(progn
     (define-arm-instruction ,name (coproc opcode-1 rd crn crm &optional opcode-2)
       ,(if two '(check-version 5))
       (let ((rd (e-translate-register rd))
             (coproc (e-translate-coproc coproc)))
         (multiple-value-bind (crn crm)
             (e-translate-coproc-registers crn crm)
           (assert (and (<= coproc #xF) (<= opcode-1 #x7)))
           (+ ,(if two '(ash #b1111 28) 0)
              (ash #b1110 24) (ash #b1 4) (ash coproc 8) (ash opcode-1 21) (ash rd 12) (ash crn 16) crm
              ,(if bit-20 '(ash 1 20) 0)
              (if opcode-2 (progn (assert (<= opcode-2 #x7)) (ash opcode-2 5)) 0)))))
     ,(if (not two) `(make-and-install-condition-fns ',name))))

(create-move-coprocessor mcr)
(create-move-coprocessor mcr2 :two t)
(create-move-coprocessor mrc :bit-20 t)
(create-move-coprocessor mrc2 :bit-20 t :two t)

;;}}}
;;{{{ instructions: miscellaneous: status register instructions

;; mrs
(progn
  (define-arm-instruction mrs (rd sr)
    (let ((rd (e-translate-register rd)))
      (assert (not (= rd 15)))
      (+ (ash #b00010 23) (ash #b001111 16) (ash rd 12)
         (ecase sr
           ('cpsr 0)
           ('spsr (ash 1 22))))))
  (make-and-install-condition-fns 'mrs))

;; msr
(defun return-sr-fields-value (sr-fields)
  (assert (string-equal sr-fields (remove-duplicates sr-fields)))
  (loop for field across sr-fields
     sum (ecase field
           (#\C (ash 1 16))
           (#\X (ash 1 17))
           (#\S (ash 1 18))
           (#\F (ash 1 19)))))

(defun encode-sr-token (sr-token)
  (let* ((sr-string (format nil "~a" sr-token))
         (sr-front (subseq sr-string 0 5))
         (sr-fields (subseq sr-string 5)))
    (+ (cond ((string-equal sr-front "CPSR-") 0)
             ((string-equal sr-front "SPSR-") (ash 1 22))
             (t (error "status register token doesnt designate cpsr- or spsr- but ~a" sr-front)))
       (return-sr-fields-value sr-fields))))

(progn
  (define-arm-instruction msr (sr-token value)
    (+ (ash #b00010010 20) (ash #b1111 12)
       (encode-sr-token sr-token)
       (cond ((symbolp value)
              (e-translate-register value))
             ((non-neg-integer-p value)
              (multiple-value-bind (immediate shifter)
                  (process-shiftable-immediate value)
                (+ (ash 1 25) (ash shifter 8) immediate)))
             (t (error "second argument ~a is neither a symbol or a positive integer" value)))))
  (make-and-install-condition-fns 'msr))

;;}}}
;;{{{ instructions: miscellaneous: multiply instructions

;; mul
(progn
  (define-arm-instruction mul (rd rm rs)
    (multiple-value-bind (rd rm rs)
        (e-translate-registers rd rm rs)
      (assert (and (not (= 15 rd)) (not (= 15 rm)) (not (= 15 rs))))
      (assert (and (not (= rd rm))))
      (+ (ash #b1001 4) (ash rd 16) (ash rs 8) rm)))
  (do-conds-and-enhance-plus-conds 'mul 's (ash #b1 20)))

;; the rest
(defmacro create-multiply-instruction (name id)
  `(progn
     (define-arm-instruction ,name (rd-lo rd-hi rm rs)
       (check-feature 'm)
       (multiple-value-bind (rd-hi rd-lo rm rs)
           (e-translate-registers rd-hi rd-lo rm rs)
         (assert (and (not (= 15 rd-hi)) (not (= 15 rd-lo)) (not (= 15 rm)) (not (= 15 rs))))
         (assert (and (not (= rd-hi rm)) (not (= rd-lo rm)) (not (= rd-hi rd-lo))))
         (+ (ash ,id 21) (ash #b1001 4) (ash rd-hi 16) (ash rd-lo 12) (ash rs 8) rm)))
     (do-conds-and-enhance-plus-conds ',name 's (ash #b1 20))))

(create-multiply-instruction smlal #b111)
(create-multiply-instruction smull #b110)
(create-multiply-instruction umlal #b101)
(create-multiply-instruction umull #b100)

;;}}}
;;{{{ instructions: miscellaneous: pseudo-instructions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun instr-spitter (&rest args)
    (list args)))

(defmacro create-pseudo-cond-loop (name)
  `(creator-loop ,name ((nil . #b1110) (EQ . #b0000) (NE . #b0001) (CS . #b0010) (HS . #b0010) (CC . #b0011) (LO . #b0011)
                        (MI . #b0100) (PL . #b0101) (VS . #b0110) (VC . #b0111) (HI . #b1000) (LS . #b1001)
                        (GE . #b1010) (LT . #b1011) (GT . #b1100) (LE . #b1101) (AL . #b1110) (NV . #b1111))))

;;adr
(defmacro create-adr-conditionals (condition code)
  (declare (ignorable code))
  `(define-arm-instruction ,(if condition (concat-symbol 'adr condition) 'adr) (rd label)
     (let ((offset (- (label-address label) (+ *here* 8))))
       (assert (and (zerop (logand offset 3)) (translate-register rd)))
       (big-endian-word-byte-list-to-nr
        (if (minusp offset)
            (%assemble (instr-spitter ',(if condition (concat-symbol 'sub condition) 'sub) rd 'pc (negate offset)))
            (%assemble (instr-spitter ',(if condition (concat-symbol 'add condition) 'add) rd 'pc offset)))))))

(create-pseudo-cond-loop create-adr-conditionals)

;; nop
(defmacro create-nop-conditionals (condition code)
  (declare (ignorable code))
  `(define-arm-instruction ,(if condition (concat-symbol 'nop condition) 'nop) ()
    (big-endian-word-byte-list-to-nr
     (%assemble (instr-spitter ',(if condition (concat-symbol 'mov condition) 'mov) 'r0 'r0)))))

(create-pseudo-cond-loop create-nop-conditionals)

;;}}}
;;{{{ instructions: miscellaneous: miscellaneous miscellaneous instructions

;; bkpt
(define-arm-instruction bkpt (immediate)
  ()
  (assert (<= immediate #xFFFF))
  (+ (ash #b111000010010 20) (ash #b0111 4) (ash (ash immediate -4) 8)
     (logand immediate #xF)))

;; clz
(progn
  (define-arm-instruction clz (rd rm)
    (check-version 5)
    (multiple-value-bind (rd rm)
        (e-translate-registers rd rm)
      (assert (and (not (= rd 15)) (not (= rm 15))))
      (+ (ash #b000101101111 16) (ash #b11110001 4) (ash rd 12) rm)))
  (make-and-install-condition-fns 'clz))

;; swi
(progn
  (define-arm-instruction swi (immediate)
    (assert (and (positive-integer-p immediate) (<= immediate #xFFFFFF)))
    (+ (ash #xF 24) immediate))
  (make-and-install-condition-fns 'swi))

;; swp
(progn
  (define-arm-instruction swp (rd rm rn)
    (multiple-value-bind (rd rm rn)
        (e-translate-registers rd rm (car rn))
      (+ (ash 1 24) (ash #b1001 4) (ash rd 12) rm (ash rn 16))))
  (do-conds-and-enhance-plus-conds 'swp 'b (ash #b1 22)))

;;}}}

;;}}}

;;}}}