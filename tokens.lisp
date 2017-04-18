(in-package :cl-minion)

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
  (append (match #\{) (collect* #'tup #\, #\}) (match #\})))

(defun arr ()
  (append (match #\[) (collect* #'val #\, #\]) (match #\])))

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
