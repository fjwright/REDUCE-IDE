;; Treat a REDUCE identifier as a word  -*- lexical-binding:t -*-
;; Like ‘superword-mode’ but handle not only ‘_’ but also ‘!’ correctly.
;; *Must* be evaluated in a REDUCE Mode buffer.
;; See ‘Word Motion’ in ELisp manual.

(setq words-include-escapes nil)

(define-key reduce-mode-map [(control shift right)] 'reduce-forward-identifier)
(define-key reduce-mode-map [(control shift left)] 'reduce-backward-identifier)

(defun reduce-forward-identifier (arg)
  "Move forwards until encountering the end of an identifier.
This includes identifiers within comments.  An identifier is a letter or
escape sequence followed by one or more alphanumeric characters or
underscores or escape sequences; an escape sequence is ‘!’ followed by
any character.  With prefix argument ARG, do it ARG times if positive,
or move backwards ARG times if negative."
  ;; cf. ‘forward-symbol’, ‘forward-word’, ‘backward-word’
  (interactive "^p")
  (cond ((> arg 0)
         (when (eq (char-before) ?!) (backward-char))
         (while
             (and
              (re-search-forward "\\(?:\\sw\\|\\s_\\|!.\\)+" nil 'move arg)
              ;; If this has found an integer then repeat:
              (string-match "\\`[[:digit:]]*\\'"
                            (match-string-no-properties 0)))))
        ((< arg 0) (reduce-backward-identifier (- arg)))))

(defun reduce-backward-identifier (arg)
  "Move backwards until encountering the beginning of an identifier.
This includes identifiers within comments.  An identifier is a letter or
escape sequence followed by one or more alphanumeric characters or
underscores or escape sequences; an escape sequence is ‘!’ followed by
any character.  With prefix argument ARG, do it ARG times if positive,
or move forwards ARG times if negative."
  ;; cf. ‘forward-symbol’, ‘forward-word’, ‘backward-word’
  (interactive "^p")
  (cond ((> arg 0)
         (while (> arg 0)
           (while
               (and
                (when (re-search-backward "\\(?:\\sw\\|\\s_\\|!.\\)+" nil 'move)
                  ;; ‘re-search-backward’ finds the match whose beginning
                  ;; is as close as possible to the starting point,
                  ;; i.e. the shortest, so...
	          (while (or (< (skip-syntax-backward "w_") 0)
                             (cond ((eq (char-before (1- (point))) ?!)
                                    (backward-char 2) t)
                                   ((eq (char-before) ?!)
                                    (backward-char) t))))
                  t)
                ;; If this has found an integer then repeat:
                (string-match "\\`[[:digit:]]*\\'"
                              (buffer-substring-no-properties
                               (point) (match-end 0)))))
           ;; A number preceding an identifier implies a product, so...
           (skip-chars-forward "[:digit:]")
           (setq arg (1- arg))))
        ((< arg 0) (reduce-forward-identifier (- arg)))))
