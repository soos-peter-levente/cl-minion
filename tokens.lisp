;; -*-  mode:lisp; coding: utf-8  -*-
;; Copyright 2017, Soós Péter Levente
;; Licensed under the MIT license.

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
  (append (match #\") (list :key (collect-char)) (match #\")))

(defun str ()
  (first (append (match #\") (list (collect-char)) (match #\"))))

(defun lit (string &optional value)
  (let ((bag (collect-literal string)))
    (or value bag)))

(defun num ()
  (or (collect-number) (fail)))
