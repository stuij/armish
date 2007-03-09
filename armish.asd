(defpackage :armish.system
  (:use :cl :asdf))

(in-package :armish.system)

(defsystem armish
  :version "0.0.1"
  :author "Ties Stuij"
  :depends-on (:arnesi :split-sequence :fiveam)
  :components ((:file "packages")
               (:file "helpers"            :depends-on ("packages"))
               (:file "assembler"          :depends-on ("helpers"))
               (:file "arm-instructions"   :depends-on ("assembler"))
               (:file "thumb-instructions" :depends-on ("assembler"))
               (:file "directives"         :depends-on ("assembler"))
               (:file "test"              :depends-on ("arm-instructions"
                                                        "thumb-instructions"
                                                        "directives"))))