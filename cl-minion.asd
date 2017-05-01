;; -*-  mode:lisp; coding: utf-8  -*-
;; Copyright 2017, Soós Péter Levente
;; Licensed under the MIT license.

(asdf:defsystem #:cl-minion
  :name "Minion parser"
  :author "Soós Péter Levente"
  :license "MIT"
  :description "A minimal JSON parser."
  :components ((:file "package")
               (:file "util")
               (:file "tokens")
               (:file "cl-minion")))
