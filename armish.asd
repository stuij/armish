(defpackage :armish.system
  (:use :cl :asdf))

(in-package :armish.system)

(defsystem armish
  :description "an arm assembler"
  :author "Ties Stuij <ties@stuij.se>"
  :license "LLGPL"
  :depends-on (:arnesi :umpa-lumpa :split-sequence :fiveam)
  :components
  ((:module :src
            :components
            ((:file "packages")
             (:file "helpers"            :depends-on ("packages"))
             (:file "assembler"          :depends-on ("helpers"))
             (:file "arm-instructions"   :depends-on ("assembler"))
             (:file "thumb-instructions" :depends-on ("assembler"))
             (:file "directives"         :depends-on ("assembler"))
             (:file "test"               :depends-on ("arm-instructions"
                                                     "thumb-instructions"
                                                     "directives"))))))