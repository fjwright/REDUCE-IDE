;;; reduce-ident.el --- Treat REDUCE identifiers as words  -*- lexical-binding:t -*-

;; Copyright (C) 2024 Francis J. Wright

;; Author: Francis J. Wright <https://sites.google.com/site/fjwcentaur>
;; Created: September 2024
;; Time-stamp: <2024-09-22 18:22:08 franc>
;; Homepage: https://reduce-algebra.sourceforge.io/reduce-ide/

;; This file is part of REDUCE IDE.

;; REDUCE IDE is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; REDUCE IDE is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with REDUCE IDE.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; *Must* be evaluated in a REDUCE Mode buffer.
;; See ‘Word Motion’ in ELisp manual.

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
  (interactive "p")
  (cond ((> arg 0)
         (when (eq (char-before) ?!) (backward-char))
         (while
             (and
              (re-search-forward "\\(?:\\sw\\|\\s_\\|!.\\)+" nil 'move arg)
              ;; If this has found an integer then try again:
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
  (interactive "p")
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
                ;; If this has found an integer then try again:
                (string-match "\\`[[:digit:]]*\\'"
                              (buffer-substring-no-properties
                               (point) (match-end 0)))))
           ;; A number preceding an identifier implies a product, so...
           (skip-chars-forward "[:digit:]")
           (setq arg (1- arg))))
        ((< arg 0) (reduce-forward-identifier (- arg)))))


;; Make word motion into identifier motion!  Like ‘superword-mode’ but
;; (mostly!) handle not only ‘_’ but also ‘!’ correctly.

(define-minor-mode reduce-identifier-mode
  "Toggle treatment of REDUCE identifiers as words.
REDUCE identifier mode is a buffer-local minor mode.  Enabling it
changes the definition of “word” to mean “REDUCE identifier” for
movement, selection, etc.  For example, ‘!*obscure!-id!-name!*’ counts
as one word."
  :lighter " !"
  (if reduce-identifier-mode
      (setq-local
       find-word-boundary-function-table
       reduce--ident-find-word-boundary-function-table)
    (kill-local-variable 'find-word-boundary-function-table)))

(defconst reduce--ident-find-word-boundary-function-table
  (make-char-table nil #'reduce--find-ident-bounday)
  "Assigned to `find-word-boundary-function-table' in
‘reduce-identifier-mode’; defers to ‘reduce--find-ident-bounday’.")

(defun reduce--find-ident-bounday (pos limit)
  "Return the position of the other identifier boundary.
Default word motion has completed.  If POS < LIMIT, then POS is at the
beginning of a default word, so return the position after the last
character of the identifier; otherwise, POS is at the last character of
a default word, so return the position of the identifier’s first
character."
  ;; cf. ‘subword-find-word-boundary’ in ‘subword.el’.
  (save-match-data
    (save-excursion
      (if (< pos limit)
          (reduce-forward-identifier 1)
        (reduce-backward-identifier 1))
      (point))))

(provide 'reduce-ident)

;;; reduce-ident.el ends here
