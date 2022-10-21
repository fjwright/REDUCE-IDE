;; An analogue of syntax-ppss for REDUCE comment statements.

(defvar-local reduce--comment-seq nil
  "List of elements of the form (start . finish).
The car of each element is the position at the start of a comment
statement and the cdr is the position immediately after the end
of that comment statement.")

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
    (setq reduce--comment-seq (nreverse lst))))

(defun reduce--highlight-comment-statements ()
  "Highlight all comment statements in the buffer.
Only works reliably with font-lock off, so turns font-lock off.
Purely intended for testing."
  (font-lock-mode 0)
  (with-silent-modifications
    (let ((lst (or reduce--comment-seq
                   (reduce--build-comment-seq))))
      (while lst
        (add-face-text-property (caar lst) (cdar lst) 'highlight)
        (setq lst (cdr lst))))))

(defun reduce--in-comment-statement-p (&optional pos)
  "Return t if POS is within a comment statement; nil otherwise.
If POS is omitted then it defaults to point."
  (unless pos (setq pos (point)))
  (let ((lst (or reduce--comment-seq
                 (reduce--build-comment-seq)))
        value)
    (while lst
      (let ((start (caar lst)) (end (cdar lst)))
        (cond ((< end pos)              ; comment before pos
               (setq lst (cdr lst)))
              ((or (eq end pos)         ; comment ends at pos
                   (< pos start))       ; pos before comment
               (setq lst nil))
              ;; start <= pos < end -- within comment
              (t (setq lst nil value t)))))
    value))
