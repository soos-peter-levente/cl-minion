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
    (t    `(:num ,(num)))))

(defun obj ()
  (append (match #\{) (lst) (match #\})))

(defun arr ()
  (append (match #\[) (list (val)) (collect* #'val #\,) (match #\])))

(defun lst ()
  (append (list (tup)) (collect* #'tup #\,)))

(defun tup ()
  (append (key) (match #\:) (val)))

(defun key ()
  (match #\") (list :key (collect-char)))

(defun str () (second (key)))

(defun lit (string &optional value)
  (let ((bag (collect-literal string)))
    (or value bag)))

(defun num ()
  (or (collect-number) (fail)))
