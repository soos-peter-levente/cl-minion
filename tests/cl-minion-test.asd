;; -*-  mode:lisp; coding: utf-8  -*-
;; Copyright 2017, Soós Péter Levente
;; Licensed under the MIT license.

(asdf:defsystem #:cl-minion-test
  :name "Minion JSON parser test suite"
  :author "Soós Péter Levvente"
  :license "MIT"
  :description "A set of regression tests for the Minion JSON parser."
  :depends-on ("cl-minion" "fiveam")
  :components ((:file "package")
               (:file "json-test-data")
               (:file "cl-minion-test")))
