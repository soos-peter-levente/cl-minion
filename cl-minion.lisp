;; -*-  mode:lisp; coding: utf-8  -*-
;; Copyright 2017, Soós Péter Levente
;; Licensed under the MIT license.

(in-package :cl-minion)

(defun parse-json (input)
  (%parse-json input))

(defun %parse-json (input)
  (cond ((streamp input) (parse-json-stream input))
        ((stringp input) (parse-json-string input))
        (t (error "Invalid input type: ~a" (type-of input)))))

(defun parse-json-string (string)
  (setf *s* (make-string-input-stream string))
  (val))

(defun parse-json-stream (stream)
  (setf *s* stream)
  (val))
