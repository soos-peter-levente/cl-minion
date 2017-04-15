(in-package :cl-minion)

(defun parse (json-string)
  (setf *s* (make-string-input-stream json-string))
  (let ((json (val)))
    (or json nil)))


