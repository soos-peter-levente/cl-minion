(asdf:defsystem #:cl-minion-test
  :name "Minion JSON parser test suite"
  :author "Soós Péter Levvente"
  :license "MIT"
  :description "A set of regression tests for the Minion JSON parser."
  :depends-on ("cl-minion" "fiveam")
  :components ((:file "cl-minion-test")))
