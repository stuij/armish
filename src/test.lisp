(in-package :armish)

(defparameter *aasm-name* #-win32 "aasm" #+win32 "aasm.exe")

(defparameter *aasm-dir* (merge-pathnames #p"aasm" 
                 (asdf:component-pathname (asdf:find-system :armish))))

;; some assemble shortcuts
(defmacro assemble-fn (name generic-special specified-special)
  `(defun ,name (fn forms)
     (let ((,generic-special ,specified-special))
       (apply fn forms))))

(assemble-fn arm-mode *mode* *arm*)
(assemble-fn thumb-mode *mode* *thumb*)
(assemble-fn arm7 *version* (set-version 4))
(assemble-fn arm9 *version* (set-version '5TE))

(defun assemble-arm7 (forms)
  (arm7 #'%assemble `(,forms)))

(defun assemble-arm9 (forms)
  (arm9 #'%assemble `(,forms)))

(defmacro assemble-curry (name ass-fn1 ass-fn2)
  `(defun ,name (forms)
     (,ass-fn1 #',ass-fn2 `(,#'%assemble (,forms)))))

(assemble-curry arm7-thumb arm7 thumb-mode)
(assemble-curry arm9-thumb arm9 thumb-mode)
(assemble-curry arm7-arm   arm7 arm-mode)
(assemble-curry arm9-arm   arm9 arm-mode)

;; some general helpers
(defun assembly-error? (output)
  (read-line output)
  (let ((error-promise (split-sequence #\space (read-line output) :remove-empty-subseqs t)))
    (if (string-not-equal (car error-promise) "List")
        (error "there's probably a syntax error in your instruction:~%~a~%~a~%" (read-line output) (read-line output)))))

(defun dissect-mode (mode)
  (let ((mode-list (split-sequence #\- (format nil "~a" mode))))
    (values (if (cadr mode-list)
                (cadr mode-list)
                'arm)
            (if (car mode-list)
                (car mode-list)
                'v5TE))))

(defun control-check (ass-instr &optional mode)
  (let ((prog-name (concatenate 'string (namestring *aasm-dir*) "/" *aasm-name*))
        (ass-file (make-pathname :directory *aasm-dir* :name "check" :type "s"))
        (lst-file (make-pathname :directory *aasm-dir* :name "check" :type "list")))
    (multiple-value-bind (mode version)
        (dissect-mode mode)
      (with-open-file (asm ass-file
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format asm "~a~%" mode)
        (format asm "arch ~a~%" version)
        (format asm "~a" ass-instr))
      #+ (and linux (or sbcl clisp allegro cmu gcl lispworks))
      (with-open-stream (output (process-output
                                 (run-prog prog-name
                                           :args (list "-l" (namestring lst-file) (namestring ass-file))
                                           :output :stream)))
        (assembly-error? output))
      #-(and linux (or sbcl clisp allegro cmu gcl lispworks))
      (warn "your implementation is not supported to give error messages from the control assembler implementation
             either because you are not on linux or because the functions to access the outer world do not support
             your implementation. Either way i just don't know how to provide for you. If you can, please fix by 
             editing run-prog and process-output in helpers.lisp and control-check in test.lisp. oh! and send a 
             fix! Thanks. Sorry for you seeing this message 100 plus times, but then again, it did get the point
             across.")
      (with-open-file (lst lst-file
                           :direction :input)
        (read-line lst) (read-line lst) ;; move along, nothing to see
        (let ((line-list (split-sequence #\space (with-standard-io-syntax
                                                   (read-line lst))
                                         :remove-empty-subseqs t)))
          (parse-integer (cadr line-list) :radix 16))))))

(defmacro instr-dump (instr &optional mode)
  `(%instr-dump ',instr ',mode))

(defun %instr-dump (instr &optional mode)
  (let* ((instr-list (funcall (case mode
                                (v5TE-arm #'arm9-arm)
                                (v5TE-thumb #'arm9-thumb)
                                (v4T-arm #'arm7-arm)
                                (v4T-thumb #'arm7-thumb)
                                (otherwise #'arm9-arm))
                              (list instr)))
         (list-nr (big-endian-word-byte-list-to-nr instr-list)))
    (values
     (trans-to-base (big-endian-word-byte-list-to-nr instr-list) 16)
     (32bit-to-nr-dump list-nr))))

(defmacro instr-control-comp (control instr &optional mode)
  `(%instr-control-comp ,control ',instr ',mode))

(defun %instr-control-comp (control instr &optional mode)
  (multiple-value-bind (nr bit-nr)
      (%instr-dump instr mode)
    (let ((control-result (control-check control mode)))
      (let* ((*print-base* 16)
             (control-nr (format nil "~a" control-result)))
        (values (list control-nr (32bit-to-nr-dump control-result) '<--cntrl)
                (list nr bit-nr '<--instr)
                (if (string-equal control-nr nr)
                    'success!! 'failure!!!!))))))

(defun arm9-arm-instr-val (instr)
  (big-endian-word-byte-list-to-nr (arm9-arm (list instr))))

(defun add-zeroes (bin-list)
  (let* ((square (length bin-list))
         (nr-zeroes (- 32 square)))
    (dotimes (i nr-zeroes bin-list)
      (setf bin-list (concatenate 'string "0" bin-list)))))

(defun 32bit-to-nr-dump (nr)
  (let ((bin-string (add-zeroes (trans-to-base nr 2))))
    (loop as i below 8
       with start = 0
       with end = 4
       collect (subseq bin-string (+ start (* i 4)) (+ end (* i 4))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun =instr (control instr mode)
    `(is (multiple-value-bind (control instr)
             (%instr-control-comp ,control ',instr ',mode)
           (string-equal (car control) (car instr)))
         "Mismatch between output of control and instruction:~%~a and ~a~%in mode ~a" ,control ',instr ',mode)))

(defmacro test-instructions (name mode &body instr-lst)
  `(test ,name
     ,@(loop for (control-instr  arm-instr) in instr-lst
          collect (=instr control-instr arm-instr mode))))

(in-suite arm-suite)

;; arm instruction tests
(test-instructions dp-instructions v5TE-arm
  ;; generic
  ("adc r2, r6, r15"            (adc r2 r6 r15))
  ("addpl r2, r6, #5"           (addpl r2 r6 5))
  ("andeqs r15, lr, r2, lsl #2" (andeqs r15 lr r2 :lsl 2))
  ("bicne r2, r12, r9, rrx"     (bicne r2 r12 r9 :rrx))
  ("eors r15, lr, r2, lsl #2"   (eors r15 lr r2 :lsl 2))
  ("orrcs r15, lr, r2, lsl #2"  (orrcs r15 lr r2 :lsl 2))
  ("rsbhs r15, lr, r2, lsl #2"  (rsbhs r15 lr r2 :lsl 2))
  ("rsccc r15, lr, r2, lsl #2"  (rsccc r15 lr r2 :lsl 2))
  ("sbclo r15, lr, r2, lsl #2"  (sbclo r15 lr r2 :lsl 2))
  ("submi r15, lr, r2, lsl #2"  (submi r15 lr r2 :lsl 2))
  ;; moving
  ("mov r1, r2"            (mov r1 r2))
  ("movvss sp, lr"         (movvss sp lr))
  ("mvnvc pc, r2, asr #31" (mvnvc pc r2 :asr 31))
  ;; comparing
  ("cmn r15, lr"           (cmn r15 lr))
  ("cmpls r2, #4"          (cmpls r2 4))
  ("teqge r2, r5, asr r9"  (teqge r2 r5 :asr r9))
  ("tstlt r15, lr"         (tstlt r15 lr)))

(test-instructions l-s-w-b v5TE-arm
  ("ldr r4, [r5, r6]"            (ldr r4 (r5 r6)))          
  ("ldrgt r1, [r12, #4]"         (ldrgt r1 (r12 4)))          
  ("ldrle r1, [r12, #-4095]"     (ldrle r1 (r12 -4095)))
  ("ldrb r5, [r2, -r4 lsl #0]"   (ldrb r5 (r2 -r4 :lsl 0)))
  ("ldrhi r1, [r2, r3 rrx]"      (ldrhi r1 (r2 r3 :rrx))) 
  ("ldrb r5, [r2, -r15]"         (ldrb r5 (r2 -r15)))
  ("ldrb r5, [r2, -r12]!"        (ldrb r5 (r2 -r12)!)) 
  ("ldrbt r5, [r2], -r1 ror #12" (ldrbt r5 (r2) -r1 :ror 12))
  ("str r5, [r2, -r12]!"         (str r5 (r2 -r12)!))
  ("strb r5, [r2, #4095]!"       (strb r5 (r2 4095)!))
  ("stralbt r5, [r2], #4095"     (stralbt r5 (r2) 4095)))

(test-instructions misc-l-s v5TE-arm
  ("ldrh r5, [r2, r3]"      (ldrh r5 (r2 r3)))
  ("ldrsh sp, [r9, #255]"   (ldrsh sp (r9 255)))
  ("ldrsb sp, [r9, #0]!"    (ldrsb sp (r9 0)!))
  ("ldrlod r0, [r4, -r6]!"  (ldrlod r0 (r4 -r6)!))
  ("strh pc, [r1], #-230"   (strh pc (r1) -230))
  ("strald r2, [pc], -pc"   (strald r2 (pc) -pc)))

(test-instructions l-s-multiple v5TE-arm
  ("ldmea r1, {r3-r5, r2}"             (ldmea r1 (r3_r5 r2)))
  ("stmib r2, {r2, r4-r5}"             (stmib r2 (r2 r4_r5)))
  ("stmgefa r2, {r15}^"                (stmgefa r2 (r15)^))
  ("ldmhied r12!, {r2, r15, r13-r14}"  (ldmhied r12! (r2 r15 r13_r14)))
  ("ldmloia r12!, {r2, r15, r13-r14}^" (ldmloia r12! (r2 r15 r13_r14)^)))

(test-instructions l-s-coprocessor v5TE-arm
  ("ldc p2, c2, [r2], #2*4"       (ldc p2 c2 (r2) 8))
  ("ldcl cp2, cr2, [r2, #0]"      (ldcl cp2 cr2 (r2 0)))
  ("stc cp2, cr2, [r2, #-255*4]!" (stc cp2 cr2 (r2 -1020)!))
  ("stc2 cp2, cr2, [r2], {23}"    (stc2 cp2 cr2 (r2) (23)))
  ("stceql p1, c15, [r15], {23}"  (stceql p1 c15 (r15) (23)))
  ("ldc2l p1, c15, [r15], {23}"   (ldc2l p1 c15 (r15) (23))))

(test-instructions coproc-instructions v5TE-arm
  ("mrc p2, 2, r2, cr2, cr3, 3"   (mrc p2 2 r2 c2 c3 3))
  ("mrc2 cp2, 2, r2, c2, cr3"     (mrc2 cp2 2 r2 cr2 c3))
  ("mcr2 cp2, 7, r2, c2, cr3, 7"  (mcr2 cp2 7 r2 cr2 c3 7))
  ("mcreq cp2, 0, r2, c2, cr3, 0" (mcreq cp2 0 r2 cr2 c3 0)))

(test-instructions status-register-instructions v5TE-arm
  ("mrslo r14, SPSR"      (mrslo r14 spsr))
  ("mrs r0, CPSR"         (mrs r0 cpsr))
  ("msr CPSR_xfsc, r1"    (msr cpsr-xfcs r1))
  ("msrhi SPSR_fs, #1008" (msrhi spsr-fs #x3F0))
  ("msrhi SPSR_xs, #0"    (msrhi spsr-xs 0)))

(test-instructions multiply v5TE-arm
  ("mul r1, r2, r3"          (mul r1 r2 r3))
  ("muls r1, r2, r3"         (muls r1 r2 r3))
  ("muleqs r1, r2, r3"       (muleqs r1 r2 r3))
  ("smlal r1, r2, r3, r4"    (smlal r1 r2 r3 r4))
  ("smullals r1, r2, r3, r4" (smullals r1 r2 r3 r4))
  ("umlaleq r1, r2, r3, sp"  (umlaleq r1 r2 r3 sp))
  ("umull r1, r2, lr, sp"    (umull r1 r2 lr sp)))

(test-instructions miscellaneous v5TE-arm
  ("bkpt 345"         (bkpt 345))
  ("clz r1, r13"      (clz r1 r13))
  ("swi 3456"         (swi 3456))
  ("swp r7, r8, [r9]" (swp r7 r8 (r9))))

;; thumb instruction tests

(test-instructions alu v5TE-thumb
  ("adc r3, r4" (adc r3 r4))
  ("and r0, r0" (and r0 r0))
  ("bic r0, r0" (bic r0 r0))
  ("cmn r7, r7" (cmn r7 r7))
  ("eor r7, r7" (eor r7 r7))
  ("mul r7, r7" (mul r7 r7))
  ("mvn r5, r4" (mvn r5 r4))
  ("neg r5, r4" (neg r5 r4))
  ("orr r5, r4" (orr r5 r4))
  ("ror r5, r4" (ror r5 r4))
  ("sbc r5, r4" (sbc r5 r4))
  ("tst r5, r4" (tst r5 r4)))

(test-instructions add-instr v5TE-thumb
  ("add r2, r9"      (add r2 r9))
  ("add sp, #12"     (add sp 12))
  ("add r2, #3"      (add r2 3))
  ("add r2, r7, #3"  (add r2 r7 3))
  ("add r2, r7, r0"  (add r2 r7 r0))
  ("add r2, pc, #16" (add r2 pc 16))
  ("add r2, sp, #16" (add r2 sp 16)))

(test-instructions sub-instr v5TE-thumb
  ("sub r1, r2, #5" (sub r1 r2 5))
  ("sub r1, #255"   (sub r1 255))
  ("sub r1, r2, r3" (sub r1 r2 r3))
  ("sub sp, #32"    (sub sp 32)))

(test-instructions shift v5TE-thumb
  ("asr r7, r6"      (asr r7 r6))
  ("asr r7, r6, #31" (asr r7 r6 31))
  ;; (instr-control-comp "asr r7, r6, #32" (asr r7 r6 32) v5TE-thumb) ctrl impl. is wrong
  ("lsr r0, r1, #2"  (lsr r0 r1 2))
  ("lsl r0, r1, #2"  (lsl r0 r1 2)))

(test-instructions cmp-instr v5TE-thumb
  ("cmp r1, #240" (cmp r1 240))
  ("cmp r1, r2"   (cmp r1 r2)))

(test-instructions mov-instr v5TE-thumb
  ("mov r4, #0"   (mov r4 0))
  ("mov r5, r6"   (mov r5 r6))
  ("mov r15, r14" (mov r15 r14))
  ("mov r15, r1"  (mov r15 r1)))

(test-instructions push/pop v5TE-thumb
  ("push {r1, r3, r2}" (push (r1 r3 r2)))
  ("push {r2, lr}"     (push (r2 lr)))
  ("pop {r0}"          (pop (r0)))
  ("pop {pc, r0}"          (pop (pc r0))))

(test-instructions load-store-multiple v5TE-thumb
  ("ldmia r3!, {r2, r1, r5}" (ldmia r3! (r2 r1 r5)))
  ("stmia r3!, {r2, r1, r5}" (stmia r3! (r2 r1 r5))))

(test-instructions load-save-word v5TE-thumb
  ("ldr r1, [r2, #40]"   (ldr r1 (r2 40)))
  ("str r1, [r2, #40]"   (str r1 (r2 40)))
  ("ldr r1, [r2, r7]"    (ldr r1 (r2 r7)))
  ("str r1, [r2, r7]"    (str r1 (r2 r7)))
  ("ldr r1, [sp, #1020]" (ldr r1 (sp 1020)))
  ("str r1, [sp, #1020]" (str r1 (sp 1020)))
  ("ldr r1, [pc, #1020]" (ldr r1 (pc 1020))))

(test-instructions load-save-halfword-byte-sign-extended-halfword-byte v5TE-thumb
  ("ldrb r1, [r2, #31]" (ldrb r1 (r2 31)))
  ("strb r1, [r2, #31]" (strb r1 (r2 31)))
  ("ldrb r1, [r2, r3]"  (ldrb r1 (r2 r3)))
  ("strb r1, [r2, r3]"  (strb r1 (r2 r3)))
  ("ldrh r1, [r2, #62]" (ldrh r1 (r2 62)))
  ("strh r1, [r2, #62]" (strh r1 (r2 62)))
  ("ldrh r1, [r2, r3]"  (ldrh r1 (r2 r3)))
  ("strh r1, [r2, r3]"  (strh r1 (r2 r3)))
  ("ldrsh r1, [r2, r3]" (ldrsh r1 (r2 r3)))
  ("ldrsb r1, [r2, r3]" (ldrsb r1 (r2 r3))))

(test-instructions misc-thumb v5TE-thumb
  ("swi 4"  (swi 4))
  ("bkpt 4" (bkpt 4)))