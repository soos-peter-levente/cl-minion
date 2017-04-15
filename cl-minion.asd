(asdf:defsystem #:cl-minion
  :name "Minion parser"
  :author "Soós Péter Levente"
  :license "MIT"
  :description "A minimal JSON parser."

  :components ((:file "package")
               (:file "util")
               (:file "cl-minion")))
