(in-package :cl-user)

(defpackage :armish
  (:use :common-lisp
        :it.bese.arnesi
        :umpa-lumpa
        :split-sequence
        :it.bese.fiveam)
  (:shadowing-import-from :it.bese.arnesi :partition)
  (:export :assemble
           :align
           :aligned
           
           :*string-end*
           :set-armish-string-encoding

           :emit-asm

           :def-asm-macro
           :def-asm-macro-lite
           :add-asm-macro
           :asm-macro-p

           :*base-address*))