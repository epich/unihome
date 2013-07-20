;; Goals:
;;   - Make it so as programmer doesn't need to pay attention to the
;;     close parens whose opening is not on the same line. Moves close
;;     parens around accordingly.
;;   - Provide an editing experience more like Python.






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

(defun adjust-close-paren-for-indent (num-close-parens)
  "Adjust NUM-CLOSE-PARENS number of close parentheses of a sexp so as
lisp-indent-adjust-sexps can indent that many levels.

 [TODO: Reword paragraph when num-close-parens implemented.]
If a close paren was moved, returns a two element list of positions:
where the close paren was moved from and the position following where
it moved to. This allows the caller to know what region potentially
needs reindentation.

If no close parens were moved, returns nil."
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

;; TODO: When the code settles, consider consolidating with
;; adjust-close-paren-for-indent
(defun adjust-close-paren-for-dedent (num-close-parens)
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
               (progn
                 (backward-sexp)
                 (point))))
          (when sexp-to-close
            (goto-char sexp-to-close)
            (forward-sexp))
          ;; Note: when no sexp-to-close found, line is empty. So put
          ;; close paren after point.
          (insert ")")
          ;; The insertion makes deleted-paren-pos off by 1
          (list (1+ deleted-paren-pos)
                (point)))))))

;; TODO: Look into how to hook into indent-for-tab-command
;; TODO: Take a region interactively: Example of expected region
;; behavior ({} indicates region boundaries)
;;     (let ((x 10) (y (some-func 20)))
;; {     (a 1)
;;       (b 2))}
;; becomes:
;;     (let ((x 10) (y (some-func 20))
;;           (a 1)
;;           (b 2)))
;; TODO: Process the prefix arg: indent that many levels, negative to
;; mean dedent
;; TODO: Write tests
(defun lisp-indent-adjust-sexps (&optional prefix-arg)
  "Indent Lisp code to the next level while adjusting sexp balanced
expressions to be consistent.

Not intended for assignment to the indent-line-function variable. "
  (interactive "P")
  (let ((orig-pos (point)))
    (back-to-indentation)
    (if (> orig-pos (point))
        ;; Effectively don't do anything so as to not obstruct TAB
        ;; completion
        (goto-char orig-pos)
      (let ((close-paren-movement
             (adjust-close-paren-for-indent prefix-arg)))
        (when close-paren-movement
          (apply 'indent-region close-paren-movement)
          ;; Like indent-for-tab-command, this command will leave
          ;; point at "back to indentation". This call is necessary
          ;; because indent-region's save-excursion marker can get
          ;; moved to the beginning of line due to how the indentation
          ;; whitespace is inserted.
          (back-to-indentation))))))

;; TODO: Investigate how to invoke this with DEL key and get old DEL
;; behavior when not dedenting (ie not in the indentation). Introduce
;; a dedent-for-del-command?
;; Note: Dedent will not take a region because:
;;   - Don't want to conflict with delete-selection-mode
;;   - Doesn't need it as much as indent with TAB does
;; TODO: Fix duplication when the code settles more
(defun lisp-dedent-adjust-sexps (&optional prefix-arg)
  (interactive "P")
  (let ((orig-pos (point)))
    (back-to-indentation)
    (if (> orig-pos (point))
        ;; Effectively don't do anything, hope to allow ordinary DEL
        (goto-char orig-pos)
      (let ((close-paren-movement
             (adjust-close-paren-for-dedent prefix-arg)))
        (when close-paren-movement
          (apply 'indent-region (nreverse close-paren-movement))
          (back-to-indentation))))))

;; TODO: Doc
;; TODO: Consider whether to consolidate with indent-for-tab-command
;; or keep separate?
(defun lisp-indent-for-tab-command (&optional arg)
  (interactive "P")
  (cond
   ;; The region is active, indent it.
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ((or ;; indent-to-left-margin is only meant for indenting,
	;; so we force it to always insert a tab here.
	(eq indent-line-function 'indent-to-left-margin)
	(and (not tab-always-indent)
	     (or (> (current-column) (current-indentation))
		 (eq this-command last-command))))
    (insert-tab arg))
   (t
    (let ((old-tick (buffer-chars-modified-tick))
          (old-point (point))
	  (old-indent (current-indentation)))

      ;; Indent the line.
      (funcall indent-line-function)

      (cond
       ;; If the text was already indented right, try completion.
       ((and (eq tab-always-indent 'complete)
             (eq old-point (point))
             (eq old-tick (buffer-chars-modified-tick)))
        (completion-at-point))

       ;; If a prefix argument was given, rigidly indent the following
       ;; sexp to match the change in the current line's indentation.
       (arg
        (let ((end-marker
               (save-excursion
                 (forward-line 0) (forward-sexp) (point-marker)))
              (indentation-change (- (current-indentation) old-indent)))
          (save-excursion
            (forward-line 1)
            (when (and (not (zerop indentation-change))
                       (< (point) end-marker))
              (indent-rigidly (point) end-marker indentation-change))))))))))

;; TODO: Create a new indent-for-del-command?
;; It should decide between functions like:
;;   lisp-dedent-adjust-sexps
;;   del-whitespace-to-tab-stop
;;   backward-delete-char-untabify

