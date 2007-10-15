(in-package :armish)

;;; Thumb assember for ARM7TDMI
;;;
;;; Created 9/13/04 by Jeff Massung
;;;
;;; THUMB.LISP
;;;

;;; annotated, expanded, safetified and bugfixed by Ties Stuij

(defmacro define-instruction (name args &body body)
  "Makes a new instruction function and adds it to the instruction set."
  `(setf (gethash ',name *thumb*) (lambda ,args ,@body)))

(defun register (reg)
  "Returns a register index and a hi/lo boolean value."
  (if (not (symbolp reg))
      nil
      (let ((local-reg (intern (symbol-name reg) :keyword)))
        (case local-reg
          (:pc (values 7 t))            ; program counter
          (:lr (values 6 t))            ; link register
          (:sp (values 5 t))            ; hardware stack pointer
          (otherwise
           (loop for r in '(:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)
              for i upfrom 0 by 1 do    ; register index
              (when (eq local-reg r)
                (if (> i 7)
                    (return (values (- i 8) t))
                    (return (values i nil))))))))))

(defun lo-reg (reg)
  "Returns a lo register (r0-r7) or nil."
  (multiple-value-bind (i hi)
      (register reg)
    (and (not hi) i)))

(defun lo-bang-reg (reg)
  (let* ((reg-string (format nil "~a" reg))
         (reg-real (intern (subseq reg-string 0 2)))
         (bang (subseq reg-string 2)))
    (assert (and (= (length reg-string) 3) (string-equal bang "!")))
    (lo-reg reg-real)))

(defun hi-reg (reg)
  "Returns a hi register (r8-r15) or nil."
  (multiple-value-bind (i hi)
      (register reg)
    (and hi i)))

(defun sp-reg (reg)
  "Returns true if the register is the stack pointer."
  (let ((local-reg (intern (symbol-name reg) :keyword)))
    (or (eq local-reg :sp)
        (eq local-reg :r13))))

(defun lr-reg (reg)
  "Returns true if the register is the link register."
  (let ((local-reg (intern (symbol-name reg) :keyword)))
    (or (eq local-reg :lr)
        (eq local-reg :r14))))

(defun pc-reg (reg)
  "Returns true if the register is the program counter."
  (let ((local-reg (intern (symbol-name reg) :keyword)))
    (or (eq local-reg :pc)
        (eq local-reg :r15))))

(defun register-list (reg-list reg-pos)
  "Returns a bit array of registers."
  (let ((bit-array 0)
        (reg-list-clean (remove-duplicates reg-list)))
    (assert (= (length reg-list-clean) (length reg-list)))
    (dolist (r reg-list bit-array)
      (incf bit-array (ash 1 (funcall reg-pos r))))))
 
(defun hi/lo (m n)
  "Returns opcode checks for rm and rn being hi/lo."
  (multiple-value-bind (rm h1) (register m)
    (multiple-value-bind (rn h2) (register n)
      (assert (or h1 h2) (h1 h2) "~a and ~a should not both be low registers" h1 h2)
      (cond ((and h1 h2) (+ #xc0 (ash rn 3) rm))
            (h1          (+ #x80 (ash rn 3) rm))
            (h2          (+ #x40 (ash rn 3) rm))))))

(defmacro define-alu-instruction (name opcode)
  "Makes a two register operand ALU instruction."
  `(define-instruction ,name (d n)
     (let ((rd (lo-reg d))
           (rn (lo-reg n)))
       (assert (and rd rn))
       (+ ,opcode (ash rn 3) rd))))

(defun check-multiple-of-4 (nr)
  (= nr (ash (ash nr -2) 2)))

;; two register, ALU instructions
(define-alu-instruction adc #x4140)     ; add with carry
(define-alu-instruction and #x4000)     ; logical and
(define-alu-instruction bic #x4380)     ; bit clear
(define-alu-instruction cmn #x42c0)     ; compare compliment
(define-alu-instruction eor #x4040)     ; exclusive or
(define-alu-instruction mul #x4340)     ; multiply
(define-alu-instruction mvn #x43c0)     ; move compliment
(define-alu-instruction neg #x4240)     ; negate
(define-alu-instruction orr #x4300)     ; inclusive or
(define-alu-instruction ror #x41c0)     ; rotate right
(define-alu-instruction sbc #x4180)     ; subtract with carry
(define-alu-instruction tst #x4200)     ; bit test

;; add register
(define-instruction add (d m &optional n)
  (if (null n)
      (cond ((sp-reg d)
             (assert (and (non-neg-int-range m #x1FC) (check-multiple-of-4 m)))
             (+ #xb000 (ash m -2)))     ; add sp,# 
            ((integerp m)
             (assert (non-neg-int-range m #xFF))
             (+ #x3000 (ash (lo-reg d) 8) m)) ; add rd,#
            (t
             (+ #x4400 (hi/lo d m))))   ; add rd,rm
      (let ((rd (lo-reg d))
            (rm (lo-reg m)))
        (assert rd)
        (if (integerp n)
            (cond ((sp-reg m)
                   (assert (and (non-neg-int-range n #x3FC) (check-multiple-of-4 n)))
                   (+ #xa800 (ash rd 8) (ash n -2))) ; add rd,sp,#
                  ((pc-reg m)
                   (assert (and (non-neg-int-range n #x3FC) (check-multiple-of-4 n)))
                   (+ #xa000 (ash rd 8) (ash n -2))) ; add rd,pc,#
                  (t
                   (assert (non-neg-int-range n #x7))
                   (+ #x1c00 (ash n 6) (ash rm 3) rd)))     ; add rd,rm,#
            (+ #x1800 (ash (lo-reg n) 6) (ash rm 3) rd))))) ; add rd,rm,rn

;; subtract register
(define-instruction sub (d m &optional n)
  (if (null n)
      (if (sp-reg d)
          (progn (assert (and (non-neg-int-range m #x1FC) (check-multiple-of-4 m)))
                 (+ #xb080 (ash m -2)))                                                ; sub sp,#
          (progn (assert (non-neg-int-range m #xFF)) (+ #x3800 (ash (lo-reg d) 8) m))) ; sub rd,#
      (let ((rd (lo-reg d))
            (rm (lo-reg m)))
        (if (integerp n)
            (progn (assert (non-neg-int-range n #x7)) (+ #x1e00 (ash n 6) (ash rm 3) rd)) ; sub rd,rm,#
            (+ #x1a00 (ash (lo-reg n) 6) (ash rm 3) rd)))))                               ; sub rd,rm,rn

;; unconditional branch
(define-instruction b (label)
  (let ((offset (- (label-address label) (+ *here* 4))))
    (assert (zerop (logand offset 1)))  ; 2-byte aligned offset
    (+ #xe000 (logand (ash offset -1) #x7ff))))

(defmacro define-conditional-branch (name condition-code)
  "Makes a conditional branch instruction."
  `(define-instruction ,name (label)
     (let ((offset (- (label-address label) (+ *here* 4))))
       (assert (zerop (logand offset 1))) ; 2-byte aligned offset
       (+ #xd000 ,condition-code (logand (ash offset -1) #xff)))))

;; conditional branches
(define-conditional-branch beq #x000)   ; zero or equal
(define-conditional-branch bne #x100)   ; not-zero or not equal
(define-conditional-branch bcs #x200)   ; carry set
(define-conditional-branch bcc #x300)   ; carry clear
(define-conditional-branch bhs #x200)   ; higher or same (unsigned)
(define-conditional-branch blo #x300)   ; lower (unsigned)
(define-conditional-branch bmi #x400)   ; minus
(define-conditional-branch bpl #x500)   ; zero or plus
(define-conditional-branch bvs #x600)   ; overflow set
(define-conditional-branch bvc #x700)   ; overflow clear
(define-conditional-branch bhi #x800)   ; higher (unsigned)
(define-conditional-branch bls #x900)   ; lower or same (unsigned)
(define-conditional-branch bge #xa00)   ; greater or equal
(define-conditional-branch blt #xb00)   ; less than
(define-conditional-branch bgt #xc00)   ; greater than
(define-conditional-branch ble #xd00)   ; less or equal

;; branch with link
(define-instruction bl (label)
  (let ((offset (- (label-address label) (+ *here* 4))))
    (assert (zerop (logand offset 1)))  ; 2-byte aligned offset
    (list (+ #xf000 (logand (ash offset -12) #x7ff))
          (+ #xf800 (logand (ash offset -1) #x7ff)))))

;; branch with link and exchange
(define-instruction blx (target)
  (check-version 5)
  (if (register target)
      (multiple-value-bind (rm hi)
          (register target)
        (assert (and (not (pc-reg target)) rm))
        (+ (ash #b010001111 7) (ash rm 3) (if hi #x40 #x00)))
      (let ((offset (- (label-address target) (+ *here* 4))))
        (assert (zerop (logand offset 1))) ; 2-byte aligned offset
        (list (+ #xf000 (logand (ash offset -12) #x7ff))
              (+ #xe800 (logand (ash offset -1) #x7ff))))))

;; branch and exchange
(define-instruction bx (m)
  (multiple-value-bind (rm hi)
      (register m)
    (assert rm)
    (+ #x4700 (ash rm 3) (if hi #x40 #x00))))

(defmacro define-shift-instruction (name reg-opcode imm-opcode)
  "Makes a shift instruction for register or immediate modes."
  `(define-instruction ,name (d n &optional m)
     (let ((rd (lo-reg d))
           (rn (lo-reg n)))
       (assert (and rd rn))
       (if (null m)
           (+ ,reg-opcode (ash rn 3) rd)
           (progn
             (assert (pos-int-range m 32))
             (+ ,imm-opcode
                (ash (if (= m 32) 0 m) 6)
                (ash rn 3) rd))))))

;; shift instructions
(define-shift-instruction asr #x4100 #x1000) ; arithmetic shift right
(define-shift-instruction lsl #x4080 #x0000) ; logical shift left
(define-shift-instruction lsr #x40c0 #x0800) ; logical shift right

;; compare register
(define-instruction cmp (m n)
  (cond ((integerp n) (assert (non-neg-int-range n #xFF)) (+ #x2800 (ash (lo-reg m) 8) n)) ; cmp rm,#
        ((and (lo-reg m) (lo-reg n)) (+ #x4280 (ash (lo-reg n) 3) (lo-reg m)))
        (t (+ #x4500 (hi/lo m n)))))    ; cmp rm,rn

;; move into register
(define-instruction mov (m n)
  (if (integerp n)
      (progn (assert (non-neg-int-range n #xFF))
             (+ #x2000 (ash (lo-reg m) 8) n)) ; mov rm,#
      (let ((rm (lo-reg m))
            (rn (lo-reg n)))
        (if (and rm rn)
            (+ #x1C00 (ash rn 3) rm)    ; mov rm,rn (both low)
            (+ (hi/lo m n) #x4600)))))  ; mov rm,rn (at least one hi)

;; load multiple registers
(define-instruction ldmia (m registers)
  (let ((rm (lo-bang-reg m)))
    (assert rm)
    (+ #xc800 (ash rm 8) (register-list registers #'lo-reg)))) ; ldmia rm!,{..}

;; store multiple registers
(define-instruction stmia (m registers)
  (let ((rm (lo-bang-reg m)))
    (assert rm)
    (+ #xc000 (ash rm 8) (register-list registers #'lo-reg)))) ; stmia rm!,{..}

;; pop registers off the stack
(define-instruction pop (registers)
  (+ #xbc00 (register-list registers #'(lambda (r)
                                         (if (pc-reg r)
                                             8
                                             (lo-reg r))))))

;; push registers onto the stack
(define-instruction push (registers)
  (+ #xb400 (register-list registers #'(lambda (r)
                                         (if (lr-reg r)
                                             8
                                             (lo-reg r))))))

;; load word
(define-instruction ldr (d m)
  (let ((rd (lo-reg d))
        (m (if (listp m) (car m) m))
        (n (if (listp m) (cadr m))))
    (assert rd)
    (cond ((integerp m)
           (let ((offset 0))
             (if (zerop *pass*)
                 (unless (member m *pool*)
                   (push m *pool*))     ; add literal to pool
                 (incf offset (+ (ash (- (pool-position) (+ *here* 2)) -2)
                                 (position m *pool*)))) ; find pc offset to literal
             (+ #x4800 (ash rd 8) offset)))             ; ldr rd,=#
          ((not (register n))
           (let ((rm (lo-reg m))
                 (i (ash n -2)))        ; 4-byte aligned
             (assert (and (non-neg-integer-p n) (zerop (logand n #x3))))
             (cond ((sp-reg m) (assert (<= i #xFF)) (+ #x9800 (ash rd 8) i))               ; ldr rd,[sp,#]
                   ((pc-reg m) (assert (<= i #xFF)) (+ #x4800 (ash rd 8) i))               ; ldr rd,[pc,#]
                   (t          (assert (<= i #x1F)) (+ #x6800 (ash i 6) (ash rm 3) rd))))) ; ldr rd,[rm,#]
          (t (+ #x5800 (ash (lo-reg m) 3) (ash (lo-reg n) 6) rd)))))                       ; ldr rd,[rm,rn]


;; load byte
(define-instruction ldrb (d m)
  (let* ((rd (lo-reg d))
         (rm (lo-reg (car m)))
         (n (cadr m))
         (rn (lo-reg n)))
    (assert (and rd rm n))
    (if (null rn)
        (progn
          (assert (and (non-neg-integer-p n) (<= n #x1F)))
          (+ #x7800 (ash n 6) (ash rm 3) rd))  ; ldrb rd,[rm,#]
        (+ #x5c00 (ash rn 6) (ash rm 3) rd)))) ; ldrb rd,[rm,rn]

;; load half-word
(define-instruction ldrh (d m)
  (let* ((rd (lo-reg d))
         (rm (lo-reg (car m)))
         (n (cadr m))
         (rn (lo-reg n)))
    (assert (and rd rm n))
    (if (null rn)
        (let ((i (ash n -1)))           ; 2-byte aligned
          (assert (and (non-neg-integer-p n) (zerop (logand n #x1)) (<= i #x1F)))
          (+ #x8800 (ash i 6) (ash rm 3) rd))  ; ldrh rd,[rm,#]
        (+ #x5a00 (ash rn 6) (ash rm 3) rd)))) ; ldrh rd,[rm,rn]

;; load sign-extended half-word
(define-instruction ldrsh (d m)
  (let ((rd (lo-reg d))
        (rm (lo-reg (car m)))
        (rn (lo-reg (cadr m))))
    (assert (and rd rm rn))
    (+ #x5e00 (ash rn 6) (ash rm 3) rd))) ; ldrsh rd,[rm,rn]

;; load sign-extended byte
(define-instruction ldrsb (d m)
  (let ((rd (lo-reg d))
        (rm (lo-reg (car m)))
        (rn (lo-reg (cadr m))))
    (assert (and rd rm rn))
    (+ #x5600 (ash rn 6) (ash rm 3) rd))) ; ldrsb rd,[rm,rn]

;; store word
(define-instruction str (d m)
  (let* ((rd (lo-reg d))
         (rm (lo-reg (car m)))
         (n (cadr m))
         (rn (lo-reg n)))
    (assert (and rd n))
    (if (null rn)
        (let ((i (ash n -2)))           ; 4-byte aligned
          (assert (and (non-neg-integer-p n) (zerop (logand n #x3))))
          (cond ((sp-reg (car m)) (assert (<= i #xFF)) (+ #x9000 (ash rd 8) i))                   ; str rd,[sp,#]
                (t                (assert (<= i #x1F)) (+ #x6000 (ash i 6) (ash rm 3) rd))))      ; str rd,[rm,#]
        (+ #x5000 (ash rn 6) (ash rm 3) rd))))                                                    ; str rd,[rm,rn]

;; store half-word
(define-instruction strh (d m)
  (let* ((rd (lo-reg d))
         (rm (lo-reg (car m)))
         (n (cadr m))
         (rn (lo-reg n)))
    (assert (and rd rm n))
    (if (null rn)
        (let ((i (ash n -1)))           ; 2-byte aligned
          (assert (and (non-neg-integer-p n) (zerop (logand n #x1)) (<= i #x1F)))
          (+ #x8000 (ash i 6) (ash rm 3) rd))  ; strh rd,[rm,#]
        (+ #x5200 (ash rn 6) (ash rm 3) rd)))) ; strh rd,[rm,rn]

;; store byte
(define-instruction strb (d m)
  (let* ((rd (lo-reg d))
         (rm (lo-reg (car m)))
         (n (cadr m))
         (rn (lo-reg n)))
    (assert (and rd rm n))
    (if (null rn)
        (progn
          (assert (and (non-neg-integer-p n) (<= n #x1F)))
          (+ #x7000 (ash n 6) (ash rm 3) rd))  ; strb rd,[rm,#]
        (+ #x5400 (ash rn 6) (ash rm 3) rd)))) ; strb rd,[rm,rn]

;; software interrupt
(define-instruction swi (n)
  (assert (non-neg-int-range n #xFF))
  (+ #xdf00 n))                         ; swi #

;; breakpoint
(define-instruction bkpt (imm)
  (check-version 5)
  (assert (non-neg-int-range imm #xFF))
  (+ (ash #b10111110 8) imm))           ; bkpt #

;; nop
(define-instruction nop ()
  (big-endian-word-byte-list-to-nr (%assemble '((mov r8 r8)))))

;; adr
(define-instruction adr (rd label)
  (if (= *pass* 0)
      0
      (let ((offset (- (label-address label) (+ *here* 4))))
        (assert (and (zerop (logand offset 3)) (non-neg-integer-p offset) (lo-reg rd)))
        (big-endian-halfword-byte-list-to-nr (%assemble `((add ,rd pc ,offset)))))))