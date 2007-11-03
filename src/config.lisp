(in-package :armish)

(defvar *string-end* 0)

(defparameter *string-encoding* :utf-8)

(defun set-armish-string-encoding (encoding)
  (setf *string-encoding* encoding))