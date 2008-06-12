(in-package :cl-user)

(defpackage :armish
  (:use :common-lisp
        :it.bese.arnesi
        :umpa-lumpa
        :split-sequence
        :it.bese.fiveam)
  (:shadowing-import-from :it.bese.arnesi :partition)
  (:export
   ;; assembler
   :assemble

   ;; config
   :*string-end*
   :set-armish-string-encoding

   ;; arm-lisp interworking
   :emit-asm

   :def-asm-space
   :in-asm-space
   :def-block
   :in-block
   :def-space-n-blocks
   :def-space-n-clusters-n-blocks
   :clear-current-block

   :def-asm-macro
   :def-asm-macro-lite
   :asm-macro-p

   :gather-code
   :gather
   :emit
   :emit-arm-fns
   :def-asm-fn-raw
   :def-asm-fn
   :def-asm
   :set-asm-init-routines
   :set-asm-final-routines

   :*base-address*))