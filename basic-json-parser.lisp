;;; PARSING UTILITIES
(defun next (stream) (peek-char nil stream nil nil))

(defun pop! (stream) (read-char stream nil nil))

(defun collect-character (stream character)
  (let ((next-character (read-char stream nil)))
    (or (when (char= next-character character)
          next-character)
        (error "Unexpected character: ~a~%" next-character))))

(defun collect-character* (stream character)
  (format t "Will collect all ~a characters from stream ~a~%" character stream))

(defun collect-character-while (stream predicate)
  (loop for current-character = (pop! stream)
     for next-character = (next stream)
     until (or (null next-character)
               (not (funcall predicate current-character)))
     collect current-character))

(defun collect-string (stream string)
  (let ((target-character-list (coerce string 'list))
        (character-buffer '()))
    (loop for target-character in target-character-list
       for next-character = (next stream)
       until (null target-character) do
         (if (char= target-character next-character)
             (progn (push next-character character-buffer) (pop! stream))
             (error "Unexpected character: ~a~%" next-character))
         :finally (return-from nil (nreverse character-buffer)))))

(defun collect-string* (stream delimiter)
  (loop for current-character = (next stream)
     until (null (next stream))
     collect (collect-character-while stream #'(lambda (character)
                                                 (not (char= character delimiter))))))

(defun collect-string-while (stream predicate) ())

(defun skip-character (stream skip-character)
  (loop for next-character = (next stream) ;until (null next-character)
     do (if (char= next-character skip-character)
            (pop! stream)
            (return-from nil next-character))))

(defun skip-character* (stream skip-character) ())

(defun skip-character-while (stream predicate)
  (loop for next-character = (next stream)
     do (cond ((funcall predicate next-character) (pop! stream))
              (t (return-from nil next-character)))))


(defun skip-n-characters (stream n)
  (loop for remainder upto (1- n)
     do (pop! stream)
     :finally (next stream)))

;;; PARSER GENERATOR
(defun sequentially-resolve-tokens (stream token-list)
  (mapcar #'(lambda (token) (funcall token stream)) token-list))

(defun conditionally-resolve-tokens (stream token-list)
  (format t "Conditionally choose from ~a for ~a~%" token-list stream))

(defun repeat-resolve-tokens (stream token-list)
  (format t "repeat-resolve-tokens ~a ~a." stream token-list))

(defun resolve-banlist (directives)
  (format t "A list of characters is returned from ~a~%" directives))

(defmacro defparser (name &rest tokens)
  (labels ((string-to-symbol (&rest arguments)
             (values (intern (apply #'(lambda (&rest args)
                                        (with-output-to-string (s)
                                          (dolist (a args) (princ a s))))
                                    arguments)))))
    `(progn
       (defparameter ,(string-to-symbol name '-grammar) ',tokens)
       ,@(loop for token-definition in tokens collect `(deftoken ,@token-definition))
       (defun ,(string-to-symbol 'parse- name) (input-string)
         (with-input-from-string (input-stream input-string)
           (value input-stream))))))

(defmacro deftoken (name token-list &optional directives)
  (labels ((has (directive) (assoc directive directives)))
    (let* ((token-function-body
            (cond ((stringp (car token-list))
                   (cond (#1=(has 'to-interal)
                             `(progn
                                (skip-string-literal stream ,(first token-list)) #1#))
                         ((has 'ignore)
                          `(progn
                             (skip-string-literal stream ,(first token-list))))
                         (t `(collect-string stream ,(first token-list)))))
                  ((has 'rep) `(repeat-resolve-tokens stream ,token-list))
                  ((has 'ignore) `(skip-character-while stream ,token-list))
                  ((has 'collect)
                   `(collect-character-while
                     stream
                     #'(lambda (character)
                         (not (member character (resolve-banlist ',(has 'ban)))))))
                  ((characterp (car token-list))
                   (cond ((has 'rep) `(collect-character* stream ,(first token-list)))
                         (t `(collect-character stream ,(first token-list)))))
                  (t `(sequentially-resolve-tokens stream ,token-list)))))
      `(defun ,name (stream)
         ,token-function-body))))

;;; JSON GRAMMAR
(defparser json
  (value           '(array-block object-block numeric-value boolean-value null-value string-literal value))
  (boolean-value   '(boolean-true boolean-false))
  (array-block     '(left-square whitespace value whitespace right-square) ((rep)))
  (object-block    '(left-curly whitespace keyword-value whitespace right-curly) ((rep)))
  (keyword-value   '(left-keyword spaces colon spaces value))
  (keyword-value*  '(keyword-value comma))
  (string-literal  '(quotes lexeme quotes))
  (quotes          '(single-quotes double-quotes))
  (lexeme          nil ((collect) (ban whitespace)))
  (text            nil ((collect) (ban quotes)))
  (boolean-true    '("true" ) ((to-internal t)))
  (boolean-false   '("false") ((to-internal nil)))
  (null-value      '("null" ) ((to-internal '())))
  (whitespace      '(spaces newline) ((ignore)))
  (spaces          '( #\space ))
  (newline         '( #\newline ))
  (single-quotes   '( #\' ))
  (double-quotes   '( #\" ))
  (colon           '( #\: ))
  (comma           '( #\, ))
  (left-square     '( #\[ ))
  (right-square    '( #\] ))
  (left-curly      '( #\{ ))
  (right-curly     '( #\} )))
