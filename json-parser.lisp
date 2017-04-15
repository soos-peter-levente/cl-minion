(defun parse-json (json-string)
  (list :json (val (make-string-input-stream json-string))))

(defun val (s)
  (case (peek-char t s nil nil)
    (#\{ (obj s))
    (#\[ (arr s))
    (#\" (key s))
    (#\t (id s "true" '(true)))
    (#\f (id s "false" '(false)))
    (#\n (id s "null" '(null)))
    (otherwise nil)))

(defun arr (s)
  (list :array
        (append (w/o-wp s (match #\[ s))
                (val s)
                (loop for rep = (match-if #\, s)
                      :while (and rep (char= rep #\,) (read-char s nil))
                      :append (val s))
                (w/o-wp s (match #\] s)))))

(defun obj (s)
  (list :object (append (w/o-wp s (match #\{ s))
                        (w/o-wp s (pairs s))
                        (w/o-wp s (match #\} s)))))

(defun pairs (s)
  (list (pair s)
        (loop for rep = (match-if #\, s)
              :while (and rep (char= rep #\,) (read-char s nil))
              :append (pair s))))

(defun pair (s)
  (list ':pair (append (w/o-wp s (key s))
                       (w/o-wp s (match #\: s))
                       (w/o-wp s (val s)))))

(defun key (s)
  (match #\" s)
  (list ':key (loop for char = (read-char s nil)
                    while (char/= char #\")
                    collect char)))

(defun id (s string value)
  (loop for e in (coerce string 'list)
        for n = (read-char s nil)
        :unless (char= e n) :do (error "Unrecognized keyword!")
        :finally (return value)))

;;;; UTILITY
(defmacro w/o-wp (stream &body body)
  ;; Wrap the read function definition BODY with peek-char calls,
  ;; so as to strip whitespace characters before and after it.
  `(progn (peek-char t ,stream nil nil)
          (let ((value ,@body))
            (peek-char t ,stream nil nil)
            value)))

(defun match (char s)
  ;; Make sure we have character CHAR in the stream up next.
  ;; Skip at the same time.
  (unless (char= char (read-char s nil))
    (error "Problem.")))

(defun match-if (char s)
  (let ((pos (peek-char t s nil nil)))
    (when pos (when (char= char pos)) pos)))

;;;;; A COUPLE OF TESTS
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

(parse-json "{\"hello\": [{\"inner\":\"depth\"}, true, false], \"there\":[\"beautiful\"]}")
