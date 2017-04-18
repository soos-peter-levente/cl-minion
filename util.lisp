(in-package :cl-minion)

(defparameter *collect-as-character-list* nil)
(defparameter *s* nil)

(defmacro w/o-wp (&body body)
  ;; Wrap the token function definition BODY with peek-char calls, so
  ;; as to strip whitespace characters in the stream before & after.
  `(progn (peek-char t *s* nil nil)
          (let ((value ,@body))
            (peek-char t *s* nil nil)
            value)))

(defun match (char)
  ;; Make sure we have character CHAR in the stream up next.  Skip it.
  (w/o-wp (if (char= char (read-char *s* nil)) (values)
              (error "Syntax Error!"))))

(defun match-if (char)
  ;; Peek ahead one character and decide if CHAR is up next.
  (let ((pos (peek-char t *s* nil nil)))
    (when pos (when (char= char pos)) pos)))

(defun collect-char ()
  ;; TODO: - support special characters, i.e. \f, \n etc.
  ;;       - add ban-bag to signal error on invalid character.
  (labels ((collect-char% ()
             (loop :for char = (read-char *s* nil)
                   :while (char/= char #\")
                   :collect char)))
    (let ((bag (collect-char%)))
      (if *collect-as-character-list* bag (coerce bag 'string)))))

(defun collect-literal (string)
  ;; Pop characters from stream as long as STRING can be read from it,
  ;; fail otherwise.
  (loop :for e in (coerce string 'list)
        :for n = (read-char *s* nil)
        :unless (char= e n) :do (error "Unrecognized keyword!")))

(defun collect* (fn separator delimiter)
  ;; Call token function FN repeatedly as long as separator is
  ;; encountered in the input stream between calls.
  (unless (char= delimiter (peek-char t *s* nil nil))
    (append (list (funcall fn))
            (loop :for rep = (match-if separator)
                  :while (and rep (char= rep separator) (read-char *s* nil))
                  :collect (funcall fn)))))

(defun collect-number ()
  ;; TODO: support scientific notation for big numbers
  (flet ((numeric-p (char) (or (digit-char-p char) (char= char #\.))))
    (read-from-string 
     (with-output-to-string (s)
       (loop :for c = (peek-char t *s* nil)
             :while (numeric-p c)
             :do  (if (numeric-p c)
                      (progn (format s "~a" c) (read-char *s* nil))
                      (error "Unrecognized number format!")))))))

(defun fail ()
  (error "Syntax error - unrecognized value type."))
