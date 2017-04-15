(in-package :cl-minion)

(defun test (fn json-string)
  (setf *s* (make-string-input-stream json-string))
  (funcall fn))

(progn
  (test #'tup  "\"hello\":\"world\"")
  (test #'tup  "\"hello\":true")
  (test #'tup  "\"hello\":false")
  (test #'tup  "\"hello\":null")
  (test #'tup  "\"hello\":{\"inner\":\"depth\"}")
  (test #'obj  "{\"hello\":\"there\"}")
  (test #'obj  "{\"hello\":\"there\",\"beautiful\":\"girl\"}")
  (test #'arr  "[true,false,\"hello\",{\"hello\":[\"there\"]}]")
  (test #'tups "\"hello\":\"there\",\"beautiful\":\"girl\""))
