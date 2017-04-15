(in-package :cl-minion)
(defparameter *s* nil)

(defun ast (json)
  (setf *s* (make-string-input-stream json)) (val))

(defun val ()
  (case (peek-char t *s* nil nil)
    (#\{  `(:obj ,(obj)))
    (#\[  `(:arr ,(arr)))
    (#\"  `(:str ,(str)))
    (#\t  `(:tru ,(lit "true"  t)))
    (#\f  `(:fal ,(lit "false" nil)))
    (#\n  `(:nul ,(lit "null"  nil)))
    (t    nil)))

(defun arr ()
  (append (match #\[) (val) (collect* #'val #\,) (match #\])))

(defun obj ()
  (append (match #\{) (lst) (match #\})))

(defun lst ()
  (append (list (tup)) (collect* #'tup #\,)))

(defun tup ()
  (append (key) (match #\:) (val)))

(defun key ()
  (match #\")
  (list :key (loop for char = (read-char *s* nil)
                   while (char/= char #\")
                   collect char)))

(defun str () (second (key)))

(defun lit (string value)
  (loop :for e in (coerce string 'list) :for n = (read-char *s* nil)
        :unless (char= e n) :do (error "Unrecognized keyword!")
        :finally (return value)))
