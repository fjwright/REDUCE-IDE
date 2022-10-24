;;; reduce-parser.el --- Parser for REDUCE comment statements  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Francis J. Wright

;; Author: Francis J. Wright <https://sites.google.com/site/fjwcentaur>
;; Created: October 2022
;; Time-stamp: <2022-10-24 17:17:49 franc>
;; Homepage: https://reduce-algebra.sourceforge.io/reduce-ide/
;; Package-Version: 1.10alpha

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

;; This file provides an analogue of syntax-ppss for REDUCE comment
;; statements, which cannot be handled by the Emacs parser.  The code
;; here builds a sequence of start and finish positions for comment
;; statements and uses it to detect efficiently whether a position is
;; within such a statement and if so to determine its start and
;; finish.

;; CURRENTLY VERY EXPERIMENTAL!

;;; Code:

(defvar-local reduce--comment-seq nil
  "A sequence of elements of the form (start . finish), or nil.
The car of each element is the position at the start of a comment
statement and the cdr is the position immediately after the end
of that comment statement.  The value is nil if there are no
comment statements, which is a common special case.")

(defun reduce--build-comment-seq ()
  "Scan the buffer for comment statements.
Use the information found to build ‘reduce--comment-seq’."
  (let ((case-fold-search t) lst)
    (save-excursion
      (save-restriction
        (widen) (goto-char (point-min))
        (save-match-data
          (while (re-search-forward "\\_<comment\\_>" nil t)
            ;; Unless in a string or syntactic comment...
            (unless (nth 8 (syntax-ppss))
              (push (cons (match-beginning 0)
                          (or (re-search-forward "[\;$]" nil t) (point-max)))
                    lst))))))
    (setq reduce--comment-seq (and lst (nreverse (vconcat lst))))))

(defun reduce--comment-seq-reset (_beg &rest _ignored-args)
  "Reset ‘reduce--comment-seq’ to nil, currently ignoring all args.
It *should* flush ‘reduce--comment-seq’ from position BEG.  The
remaining arguments, IGNORED-ARGS, are ignored; this function
accepts them so that it can be directly used on the hook
‘before-change-functions’, which is set up  in ‘reduce-mode’."
  (setq reduce--comment-seq nil))

(defun reduce--highlight-comment-statements ()
  "Highlight all comment statements in the buffer.
Only works reliably with font-lock off, so turns font-lock off.
Purely intended for testing."
  (interactive)
  (let ((seq (or reduce--comment-seq (reduce--build-comment-seq))))
    (when seq                           ; nil if no comment statements
      (font-lock-mode 0)
      (with-silent-modifications
        (mapc
         (lambda (i)                    ; i = (start . finish)
           (add-face-text-property (car i) (cdr i) 'highlight))
         seq)))))

(defun reduce--in-comment-statement-p (&optional pos)
  "Return a cons if POS is within a comment statement; nil otherwise.
If POS is omitted then it defaults to point.  The cons has the
form (start . finish), where start is the position at the start
of the comment statement containing POS and finish is the
position immediately after the end of that comment statement."
  (interactive)
  (let ((seq (or reduce--comment-seq (reduce--build-comment-seq))))
    (when seq                           ; nil if no comment statements
      (unless pos (setq pos (point)))
      (let ((lower 0) (upper (1- (length seq))) value)
        ;; seq is a sequence of integer intervals.  Use bisection to
        ;; search seq for an interval surrounding POS.  lower < mid <
        ;; upper are indices into seq and mid is the (integer) mid-point
        ;; between lower and upper.  ivl is the mid-point interval.
        (while (<= lower upper)
          (let* ((mid (/ (+ lower upper) 2)) (ivl (elt seq mid))
                 (start (car ivl)) (finish (cdr ivl)))
            (cond ((< finish pos)       ; interval < pos
                   (setq lower (1+ mid)))
                  ((< pos start)        ; pos < interval
                   (setq upper (1- mid)))
                  ((eq finish pos)      ; pos = interval-finish
                   (setq lower (1+ upper))) ; -- stop loop
                  ;; interval-start <= pos < interval-finish
                  ;; so pos within interval -- stop loop
                  (t (setq lower (1+ upper) value ivl)))))
        (when (called-interactively-p 'interactive) (message "%s" value))
        value))))


;;;; *****************************************************************
;;;; Searching for syntactic elements ignoring comments, strings, etc.
;;;; *****************************************************************

(defun reduce--re-search-forward (regexp &optional MOVE)
  "Syntactic search forwards for REGEXP.
Skip comments, strings, escaped and quoted tokens.  If match
found then move point to end of match and return t, else return
nil.  If no match found and MOVE is non-nil then move point as
far as possible, otherwise do not move point at all."
  (let ((start (point))
        (move (if MOVE 'move t)))
    (if (reduce--re-search-forward1 regexp move)
        t
      (unless MOVE (goto-char start))
      nil)))

(defun reduce--re-search-forward1 (regexp move)
  "Search forwards for REGEXP; if no match and MOVE then move to EOB.
Recursive sub-function of ‘reduce--re-search-forward’.
Process match to skip comments, strings, etc.
Return t if match found, nil otherwise."
  (when (re-search-forward regexp nil move)
    (let (tmp)
      (if (cond                     ; check match -- t => search again
           ((memq (char-before (match-beginning 0)) '(?! ?'))) ; escaped or quoted
           ((nth 3 (setq tmp (syntax-ppss))) ; in string
            (goto-char (nth 8 tmp))
            (forward-sexp) t)
           ((nth 4 tmp)                 ; in % or /*...*/ comment
            (goto-char (nth 8 tmp))
            (forward-comment 1) t)
           ((setq tmp (reduce--in-comment-statement-p
                       (match-beginning 0))) ; comment statement
            (goto-char (cdr tmp)) t))
          (reduce--re-search-forward1 regexp move) ; search again
        t))))                         ; match for original regexp found


(defun reduce--re-search-backward (regexp &optional MOVE)
  "Syntactic search backwards for REGEXP.
Skip comments, strings, escaped and quoted tokens.  If match
found then move point to beginning of match and return t, else
return nil.  If no match found and MOVE is non-nil then move
point as far as possible, otherwise do not move point at all."
  (let ((start (point))
        (move (if MOVE 'move t)))
    (if (reduce--re-search-backward1 regexp move)
        t
      (unless MOVE (goto-char start))
      nil)))

(defun reduce--re-search-backward1 (regexp move)
  "Search backwards for REGEXP; if no match and MOVE then move to BOB.
Recursive sub-function of ‘reduce--re-search-backward’.
Process match to skip comments, strings, etc.
Return t if match found, nil otherwise."
  (when (re-search-backward regexp nil move)
    (let (tmp)
      (if (cond                     ; check match -- t => search again
           ((memq (preceding-char) '(?! ?'))) ; escaped or quoted
           ((setq tmp (nth 8 (syntax-ppss)))
            ;; in string or % or /**/ comment
            (goto-char tmp) t)          ; skip it
           ((setq tmp (reduce--in-comment-statement-p))
            ;; in comment statement
            (goto-char (car tmp)) t))               ; skip it
          (reduce--re-search-backward1 regexp move) ; search again
        t))))                         ; match for original regexp found


;;;; **********************************************
;;;; Skipping comments and white space of all types
;;;; **********************************************

(defun reduce--skip-comments-forward ()
  "Move forwards across comments and white space of all types."
  (let (cmnt)
    (forward-comment (buffer-size)) ; syntactic comments & white space
    (while (setq cmnt (reduce--in-comment-statement-p))
      (goto-char (cdr cmnt))
      (forward-comment (buffer-size))))) ; syntactic comments & white space

(defun reduce--skip-comments-backward ()
  "Move backwards across comments and white space of all types."
  (let (cmnt)
    (forward-comment (-(buffer-size))) ; syntactic comments & white space
    (while (setq cmnt (reduce--in-comment-statement-p (1- (point))))
      (goto-char (car cmnt))
      (forward-comment (-(buffer-size)))))) ; syntactic comments & white space

(provide 'reduce-parser)

;;; reduce-parser.el ends here
