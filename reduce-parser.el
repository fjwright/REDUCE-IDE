;; An analogue of syntax-ppss for REDUCE comment statements.

(defvar-local reduce--comment-alist nil
  "Alist of elements of the form (position . in).
The car is the position at the start or immediately after the end
of a comment statement, and the cdr is t at the start and nil at
the end.")

(defun reduce--update-comment-alist ()
  "Scan the buffer for comment statements.
Use the information found to update ‘reduce--comment-alist’."
  (let ((case-fold-search t) alist)
    (save-excursion
      (save-restriction
        (widen) (goto-char (point-min))
        (while (re-search-forward "\\_<comment\\_>" nil t)
          ;; Unless in a string or syntactic comment...
          (unless (nth 8 (syntax-ppss))
            (push (cons (match-beginning 0) t) alist)
            (when (re-search-forward "[\;$]" nil t)
              (push (cons (match-end 0) nil) alist))))))
    (setq reduce--comment-alist (reverse alist))))

(defun reduce--highlight-comment-statements ()
  "Highlight all comment statements in the buffer.
Only works reliably with font-lock off, so turn font-lock off.
Purely intended for testing."
  (font-lock-mode 0)
  (with-silent-modifications
    (let ((alist (or reduce--comment-alist
                     (reduce--update-comment-alist))))
      (while alist
        (let ((start (caar alist))
              (end (or (caadr alist) (point-max))))
          (setq alist (cddr alist))
          (add-face-text-property start end 'highlight))))))
