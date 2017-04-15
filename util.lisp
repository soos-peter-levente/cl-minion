(in-package :cl-minion)

(defmacro w/o-wp (&body body)
  ;; Wrap the token function definition BODY with peek-char calls, so
  ;; as to strip whitespace characters in the stream before & after.
  `(progn (peek-char t *s* nil nil)
          (let ((value ,@body))
            (peek-char t *s* nil nil)
            value)))

(defun match (char)
  ;; Make sure we have character CHAR in the stream up next.  Skip it.
  (w/o-wp (or (char= char (read-char *s* nil)) (error "Problem."))))

(defun match-if (char)
  ;; Peek ahead one character and decide if CHAR is up next.
  (let ((pos (peek-char t *s* nil nil)))
    (when pos (when (char= char pos)) pos)))

(defun collect-char ()
  (loop :for char = (read-char *s* nil)
        :while (char/= char #\")
        :collect char))

(defun collect-literal (string)
  ;; Pop characters from stream as long as STRING can be read from it,
  ;; fail otherwise.
  (loop :for e in (coerce string 'list)
        :for n = (read-char *s* nil)
        :unless (char= e n) :do (error "Unrecognized keyword!")))

(defun collect* (fn separator)
  ;; Call token function FN repeatedly as long as separator is
  ;; encountered in the input stream between calls.
  (loop :for rep = (match-if separator)
        :while (and rep (char= rep separator) (read-char *s* nil))
        :collect (funcall fn)))
