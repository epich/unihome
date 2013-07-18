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
where the close paren was moved from and moved to. This allows the
caller to know what region potentially needs reindentation.

If no close parens were moved, returns nil."
  (save-excursion
    (let (;; Position of deleted close paren or nil
          (pos-of-deletion (save-excursion
                             (beginning-of-line)
                             (backward-sexp)
                             ;; If the sexp at point is a list,
                             ;; delete its closing paren
                             (when (eq (scan-lists (point) 1 0)
                                       (scan-sexps (point) 1))
                               (forward-sexp)
                               (delete-char -1)
                               (point)))))
      (when pos-of-deletion
        (let ((sexp-to-close
              (last-sexp-with-relative-depth (point)
                                             (progn (end-of-line) (point))
                                             0)))
          (when sexp-to-close
            (goto-char sexp-to-close)
            (forward-sexp))
          ;; Note: when no sexp-to-close found, line is empty. So put
          ;; close paren after point.
          (prog1
              ;; Return where close paren moved from and to
              (list pos-of-deletion (point))
            (insert ")")))))))

;; TODO: How to hook into indent-for-tab-command?
;; TODO: Take a region interactively: Example of expected region behavior ({} indicates region boundaries)
;;     (let ((x 10) (y (some-func 20)))
;; {     (a 1)
;;       (b 2))}
;; becomes:
;;     (let ((x 10) (y (some-func 20))
;;           (a 1)
;;           (b 2)))
;; TODO: Process the prefix arg: indent that many levels, negative to mean dedent
;; TODO: Write tests
(defun lisp-indent-adjust-sexps (&optional prefix-arg)
  "Indent Lisp code to the next level while adjusting sexp balanced
expressions to be consistent.

Not intended for assignment to the indent-line-function variable. "
  (interactive "P")
  (let ((orig-pos (point)))
    (back-to-indentation)
    (if (> orig-pos (point))
        ;; Effectively don't do anything so as to not obstruct completion
        (goto-char orig-pos)
      (let ((close-paren-movement (adjust-close-paren-for-indent prefix-arg)))
        (when close-paren-movement
          (apply 'indent-region close-paren-movement)
          ;; Like indent-for-tab-command, this command will leave
          ;; point at "back to indentation". This call is necessary
          ;; because indent-region's save-excursion marker can get
          ;; moved to the beginning of line due to how the indentation
          ;; whitespace is inserted.
          (back-to-indentation))))))

;; TODO: Create a new indent-for-del-command?

;; If there is non whitespace before point on the same line (see back-to-indentation):
;;   Resort to whatever DEL would normally do instead
;; Else if at the beginning of line already:
;;   Resort to whatever DEL would normally do instead
;; Else find last close paren on same line as point
;;   - Delete close paren and move to end of previous line
;;     - Find close paren:
;;       - Use parse-partial-sexp to parse from beginning to end of line
;;         - Reference how move-past-close-and-reindent does it
;;       - parse-partial-sexp returns information about beginning of comment, navigate there
;;       - From there, if no error, backward-list forward-list I think will take me to the desired close paren
;;         - But before forward-list, verify backward-list took us to the previous line, not the current one
;;   - Indent (indent-for-tab-command ?)
(defun adjust-close-paren-for-dedent (&optional prefix-arg)
  (interactive "P")
  )

;; Note: When | point, what to do when DEL?
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               |(format "foo") "bar"
               "baz")))
;; Proposition: For simplicity, have rule that if multi-line close paren not on same line as point, do ordinary DEL

;; TODO: Account for this case when | point, then DEL
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               "abc"
               |(format "foo") "bar") (progn))) ; NB: (progn) is outside of message, but formatting suggests otherwise
;; Possibility 1: violates invariant, see NB
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               "abc")
      |(format "foo" "bar") (progn))) ; NB: Violates principle of only deleting close parens whose opening is not on the same line
;; Possibility 2: allows TAB to be an inverse
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               "abc")
      |(format "foo") "bar") (progn)) ; NB: (progn) is outside of let, but formatting suggests otherwise
;; Possibility 3: TAB is not an inverse
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               "abc")
      |(format "foo") "bar" (progn))) ; NB: Take the close paren that matches to the first open paren of the previous line
;; TODO: Possibility 2 and TAB
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               "abc"
               |(format "foo") "bar") (progn))) ; NB: Back to previous state
;; TODO: Possibility 3 and TAB
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               "abc"
               |(format "foo") "bar" (progn)))) ; NB: Now (progn) is inside message form when it wasn't before
                                                ; (The price paid for badly placed (progn))?

;; Note: When | point
(if cond
    (progn (expr-1
            |) (expr-2))
  else)
;; Then DEL :
(if cond
    (progn (expr-1)
           (expr-2))
  else)

;; Note: When | point, what to do when DEL?
(if cond
    (progn (expr-1
    |
            ) (expr-2))
  else)
;; Possibility 1:
(if cond
    (progn (expr-1)
           |(expr-2))
  else)
;; Possibility 2: Do as DEL would otherwise do
(if cond
    (progn (expr-1
   |
            ) (expr-2))
  else)

;; If TAB, what to do?
(if cond
    (progn (lambda () x)
           |foo) bar
  y)
;; We know we want foo to indent to the right. Whether the close
;; paren is after 'foo)' or after 'bar' is of no significance.

