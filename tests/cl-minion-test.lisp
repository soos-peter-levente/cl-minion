;; -*-  mode:lisp; coding: utf-8  -*-
;; Copyright 2017, Soós Péter Levente
;; Licensed under the MIT license.

(in-package :cl-minion-test)

(def-suite cl-minion-test
  :description "Regression testing suite for Minion JSON parser.")

(in-suite cl-minion-test)

(test cl-minion-keywords
  ;; parse null  
  (is (parse-json "{\"null\":null}")
      '(:OBJ ((:KEY "hello" :NUL NIL))))
  ;; parse true
  (is (parse-json "{\"true\":true}")
      '(:OBJ ((:KEY "test" :TRU T))))
  ;; parse false
  (is (parse-json "{\"false\":false}")
      '(:OBJ ((:KEY "test" :FAL NIL))))
  ;; parse with whitespace around colon
  (is (parse-json "{\"keyword\"    :    \"value\"}")
      '(:OBJ ((:KEY "keyword" :STR "value")))))

(test cl-minion-strings
  ;; parse keyword with space
  (is (parse-json "{\"keyword with space\":true}")
      '(:OBJ ((:KEY "keyword with space" :TRU T))))
  ;; parse string literal whitespace ; TODO!!
  (is (parse-json "{\"keyword\":\"\"}")
      '(:OBJ ((:KEY "keyword" :STR ""))))
  ;; parse string literal with whitespace
  (is (parse-json "{\"keyword\":\"hello there\"}")
      '(:OBJ ((:KEY "keyword" :STR "hello there"))))
  ;; parse string w/o spaces
  (is (parse-json "{\"hello\":\"world\"}")
      '(:OBJ ((:KEY "hello" :STR "world"))))
  ;; parse string with spaces
  (is (parse-json "{\"hello\":\"world, how are you\"}")
      '(:OBJ ((:KEY "hello" :STR "world, how are you")))))

(test cl-minion-arrays
    ;; parse empty array
  (is (parse-json "{\"empty-array\":[]}")
      '(:OBJ ((:KEY "empty-array" :ARR NIL))))
  ;; parse single-element array
  (is (parse-json "{\"single-element-array\":[[{}]]}")
      '(:OBJ ((:KEY "single-element-array" :ARR ((:ARR ((:OBJ NIL))))))))
  ;; parse multi-element array
  (is (parse-json "[1,2,3,-23411234,4,-5.67,{\"obj\":\"val\"},[1,2,3]]")
      '(:ARR ((:NUM 1) (:NUM 2) (:NUM 3) (:NUM -23411234) (:NUM 4) (:NUM -5.67)
              (:OBJ ((:KEY "obj" :STR "val"))) (:ARR ((:NUM 1) (:NUM 2) (:NUM 3))))))
  ;; parse with whitespace
  (is (parse-json "[1  , 2, 34, \"hello\", {   }, [ ], -456.43, 56]")
      '(:ARR ((:NUM 1) (:NUM 2) (:NUM 34) (:STR "hello") (:OBJ NIL) (:ARR NIL)
             (:NUM -456.43) (:NUM 56))))
  ;; parse nested array
  (is (parse-json "{\"nested-array\":[[[1]]]}")
      '(:OBJ ((:KEY "nested-array" :ARR ((:ARR ((:ARR ((:NUM 1)))))))))))

(test cl-minion-objects
  ;; parse empty object
  (is (parse-json "{}")
      '(:OBJ NIL))
  ;; parse nested objects
  (is (parse-json "{\"object\":{\"nested\":{\"nested-object\":\"value\"}}}")
      '(:OBJ ((:KEY "object"
              :OBJ ((:KEY "nested" :OBJ ((:KEY "nested-object" :STR "value")))))))))

(test cl-minion-numbers
  ;; parse small integer
  (is (parse-json "{\"small\":66}")
      '(:OBJ ((:KEY "small" :NUM 66))))
  ;; parse negative integer
  (is (parse-json "{\"negative-integer\":-66}")
      '(:OBJ ((:KEY "negative-integer" :NUM -66))))
  ;; parse large integer
  (is (parse-json "{\"large\":4611686018427387904}")
      '(:OBJ ((:KEY "large" :NUM 4611686018427387904}))))
  ;; parse float
  (is (parse-json "{\"float\":[45.67,-67.89]}")
      '(:OBJ ((:KEY "float" :ARR ((:NUM 45.67) (:NUM -67.89)))))))

(test cl-minion-all
  (is (every #'consp (mapcar #'parse-json *json-test-data*))))

(defun run-tests ()
  (run! 'cl-minion-test))
