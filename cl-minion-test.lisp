(in-package :cl-minion)

(defun test (fn json-string)
  (let ((s (make-string-input-stream json-string)))
    (funcall fn s)))

(progn
  (test #'pair  "\"hello\":\"world\"")
  (test #'pair  "\"hello\":true")
  (test #'pair  "\"hello\":false")
  (test #'pair  "\"hello\":null")
  (test #'pair  "\"hello\":{\"inner\":\"depth\"}")
  (test #'obj   "{\"hello\":\"there\"}")
  (test #'obj   "{\"hello\":\"there\",\"beautiful\":\"girl\"}")
  (test #'arr   "[true,false,\"hello\",{\"hello\":\"there\"}]")
  (test #'pairs "\"hello\":\"there\",\"beautiful\":\"girl\""))
