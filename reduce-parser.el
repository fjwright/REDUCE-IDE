;;; reduce-parser.el --- Parser for REDUCE comment statements  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Francis J. Wright

;; Author: Francis J. Wright <https://sites.google.com/site/fjwcentaur>
;; Created: October 2022
;; Time-stamp: <2022-10-23 17:42:49 franc>
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
        (while (re-search-forward "\\_<comment\\_>" nil t)
          ;; Unless in a string or syntactic comment...
          (unless (nth 8 (syntax-ppss))
            (push (cons (match-beginning 0)
                        (or (re-search-forward "[\;$]" nil t) (point-max)))
                  lst)))))
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

(provide 'reduce-parser)

;;; reduce-parser.el ends here
