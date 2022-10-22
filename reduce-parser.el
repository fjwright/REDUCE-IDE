;; An analogue of syntax-ppss for REDUCE comment statements.

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

(defun reduce--highlight-comment-statements ()
  "Highlight all comment statements in the buffer.
Only works reliably with font-lock off, so turns font-lock off.
Purely intended for testing."
  (let ((seq (or reduce--comment-seq (reduce--build-comment-seq))))
    (when seq                           ; nil if no comment statements
      (font-lock-mode 0)
      (with-silent-modifications
        (mapc
         (lambda (i)                    ; i = (start . finish)
           (add-face-text-property (car i) (cdr i) 'highlight))
         seq)))))

(defun reduce--in-comment-statement-p (&optional pos)
  "Return t if POS is within a comment statement; nil otherwise.
If POS is omitted then it defaults to point."
  (let ((seq (or reduce--comment-seq (reduce--build-comment-seq))))
    (when seq                           ; nil if no comment statements
      (unless pos (setq pos (point)))
      (let ((lower 0) (upper (1- (length seq))) value (count 0))
        ;; seq is a sequence of integer intervals.  Use bisection to
        ;; search seq for an interval surrounding POS.  lower < mid <
        ;; upper are indices into seq and mid is the (integer) mid-point
        ;; between lower and upper.  ivl is the mid-point interval.
        (while (<= lower upper)
          ;; (setq count (1+ count))
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
                  (t (setq lower (1+ upper) value t)))))
        ;; (message "intervals: %s; searches: %s" (length seq) count)
        value))))
