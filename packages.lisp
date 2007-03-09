(in-package :cl-user)

(defpackage :armish
  (:use :common-lisp
        :it.bese.arnesi
        :split-sequence
        :it.bese.fiveam)
  (:shadowing-import-from :it.bese.arnesi :partition)
  (:export :assemble
           :align
           :aligned
           :nr-to-big-endian-word-byte-list
           :concat-symbol))