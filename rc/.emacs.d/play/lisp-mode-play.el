;; Goals:
;;   - Make it so as programmer doesn't need to pay attention to the
;;     close parens whose opening is not on the same line. Moves close
;;     parens around accordingly.
;;   - Provide an editing experience more like Python.

;; TODO: Document thoughts on taking a region, including why dedent doesn't need it.
;; TODO: Have work with Clojure's [] and {} delimiters
;; TODO: Write tests

(require 'cl)

(defun last-sexp-with-relative-depth (from-pos to-pos rel-depth)
  "Parsing sexps from FROM-POS (inclusive) to TO-POS (exclusive),
return the position of the last sexp that had depth REL-DEPTH relative
to FROM-POS. Returns nil if REL-DEPTH is not reached.

Examples:
  Region:   a (b c (d)) e (f g (h i)) j

  Evaluate: (last-sexp-with-relative-depth pos-a (1+ pos-j) 0)
  Returns:  position of j

  Evaluate: (last-sexp-with-relative-depth pos-a (1+ pos-j) -1)
  Returns:  position of (h i)

This function assumes FROM-POS is not in a string or comment."
  (save-excursion
    (goto-char from-pos)
    (let (the-last-pos
          (parse-state '(0 nil nil nil nil nil nil nil nil)))
      (while (< (point) to-pos)
        (setq parse-state
              (parse-partial-sexp (point)
                                  to-pos
                                  nil
                                  t ; Stop before sexp
                                  parse-state))
        (and (not (eq (point) to-pos))
             (eq (car parse-state) rel-depth)
             (setq the-last-pos (point)))
        ;; The previous parse may not advance. To advance and maintain
        ;; correctness of depth, we parse over the next char.
        (setq parse-state
              (parse-partial-sexp (point)
                                  (1+ (point))
                                  nil
                                  nil
                                  parse-state)))
      the-last-pos)))

(defun adjust-close-paren-for-indent ()
  "Adjust a close parentheses of a sexp so as
lisp-indent-adjust-parens can indent that many levels.

If a close paren was moved, returns a two element list of positions:
where the close paren was moved from and the position following where
it moved to.

If there's no close parens to move, either return nil or allow
scan-error to propogate up."
  (save-excursion
    (let ((deleted-paren-pos
           (save-excursion
             (beginning-of-line)
             (backward-sexp)
             ;; If the sexp at point is a list,
             ;; delete its closing paren
             (when (eq (scan-lists (point) 1 0)
                       (scan-sexps (point) 1))
               (forward-sexp)
               (delete-char -1)
               (point)))))
      (when deleted-paren-pos
        (let ((sexp-to-close
               (last-sexp-with-relative-depth (point)
                                              (progn (end-of-line)
                                                     (point))
                                              0)))
          (when sexp-to-close
            (goto-char sexp-to-close)
            (forward-sexp))
          ;; Note: when no sexp-to-close found, line is empty. So put
          ;; close paren after point.
          (insert ")")
          (list deleted-paren-pos (point)))))))

(defun adjust-close-paren-for-dedent ()
  "Adjust a close parentheses of a sexp so as
lisp-dedent-adjust-parens can dedent that many levels.

If a close paren was moved, returns a two element list of positions:
where the close paren was moved from and the position following where
it moved to.

If there's no close parens to move, either return nil or allow
scan-error to propogate up."
  (save-excursion
    (let ((deleted-paren-pos
           (save-excursion
             (when (< (point)
                      (progn (up-list)
                             (point)))
               (delete-char -1)
               (point)))))
      (when deleted-paren-pos
        (let ((sexp-to-close
               ;; Needs to work when dedenting in an empty list, in
               ;; which case backward-sexp will signal scan-error and
               ;; sexp-to-close will be nil.
               (condition-case nil
                   (progn (backward-sexp)
                          (point))
                 (scan-error nil))))
          ;; Move point to where to insert close paren
          (if sexp-to-close
              (forward-sexp)
            (backward-up-list)
            (forward-char 1))
          (insert ")")
          ;; The insertion makes deleted-paren-pos off by 1
          (list (1+ deleted-paren-pos)
                (point)))))))

(defun adjust-parens-p ()
  "Whether to adjust parens."
  (save-excursion
    (let ((orig-pos (point)))
      (back-to-indentation)
      (and (not (use-region-p))
           (<= orig-pos (point))))))

(defun adjust-parens-and-indent (adjust-function prefix-arg)
  "Adjust close parens and indent the region over which the parens
moved."
  (let ((region-of-change (list (point) (point))))
    (cl-loop for i from 1 to (or prefix-arg 1)
             with finished = nil
             while (not finished)
             do
             (condition-case err
                 (let ((close-paren-movement
                        (funcall adjust-function)))
                   (if close-paren-movement
                       (setq region-of-change
                             (list (min (car region-of-change)
                                        (car close-paren-movement)
                                        (cadr close-paren-movement))
                                   (max (cadr region-of-change)
                                        (car close-paren-movement)
                                        (cadr close-paren-movement))))
                     (setq finished t)))
               (scan-error (setq finished err))))
    (apply 'indent-region region-of-change))
  (back-to-indentation))

(defun lisp-indent-adjust-parens (&optional prefix-arg)
  "Indent Lisp code to the next level while adjusting sexp balanced
expressions to be consistent.

This command can be bound to TAB instead of indent-for-tab-command. It
potentially calls the latter."
  (interactive "P")
  (if (adjust-parens-p)
      (adjust-parens-and-indent 'adjust-close-paren-for-indent
                                prefix-arg)
    (indent-for-tab-command prefix-arg)))

(defun lisp-dedent-adjust-parens (&optional prefix-arg)
  "Dedent Lisp code to the previous level while adjusting sexp
balanced expressions to be consistent.

Binding to <backtab> (ie Shift-Tab) is a sensible choice."
  (interactive "P")
  (when (adjust-parens-p)
    (adjust-parens-and-indent 'adjust-close-paren-for-dedent
                              prefix-arg)))




