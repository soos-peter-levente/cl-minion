(in-package :cl-minion)

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
