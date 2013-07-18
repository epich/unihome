;; Goals:
;;   - Make it so as programmer doesn't need to pay attention to the
;;     close parens whose opening is not on the same line. Moves close
;;     parens around accordingly.
;;   - Provide an editing experience more like Python.

;; TODO: Hook into lisp-indent-command instead of calling indent-for-tab-command
;; (The latter calls the former.)

(defun last-sexp-with-relative-depth (from-pos to-pos rel-depth)
  "Parsing sexps from FROM-POS to TO-POS, return the position of the
last sexp with depth REL-DEPTH relative to the first sexp.

Returns nil if rel-depth is not reached."
  (save-excursion
    (goto-char from-pos)
    (let (the-last-sexp
          (parse-state `(0 nil nil
                           ,(null (calculate-lisp-indent))
                           nil nil nil nil nil)))
      (while (< (point) to-pos)
        (when (eq (car parse-state) rel-depth)
          (setq the-last-sexp (point)))
        (setq parse-state
              (parse-partial-sexp (point)
                                  (1+ (point))
                                  nil
                                  nil
                                  parse-state)))
      the-last-sexp)))

;; TODO: Testing
;;a (b c (d)) e (f g (h i)) (j)
;;(last-sexp-with-relative-depth 6884 6913 0)

;; If there is non whitespace before point on the same line (see back-to-indentation):
;;   Resort to whatever TAB would normally do instead
;; Else if the end of previous line does not end with a close paren:
;;   Resort to whatever TAB would normally do instead
;; Else:
;;   - Delete last close paren
;;     - Find close paren:
;;       - Go to beginning of line so as not in a comment
;;       - backward-list forward-list
;;   - Place close paren after last sexp on the line with depth 0 relative to start of line
;;     (could be several lines later if sexp is a list)
;;   - Or: Place close paren in new location
;;     - Parse the current line
;;       - If ends with depth 0 or less, place close paren at end of last sexp on line
;;       - Else, backup the opening of a list with depth 0
;;   - If the char after point is not one of:
;;       ) ] } whitespace end-of-line
;;     then put a space between the close paren and it
;;     TODO: Look at insert-parentheses to see what it does for inserting space
;;   - Indent
(defun forward-indent-adjust-sexp (&optional prefix-arg)
  ;; TODO: Document. Include the return of a region that needs reindentation
  (interactive "P")
  (save-excursion
    ;; TODO: Maybe don't worry about this case, just go to beginning of line anyway and proceed
    (if (> (point) (progn (back-to-indentation) (point)))
        nil ; Point is after indentation on the line, do nothing
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
          (let* (
                 ;; Now "back to indentation", parse from here to end of
                 ;; line to find where to place the deleted close paren
                 (pos-of-indent (point))
                 (end-of-line (progn (end-of-line) (point)))
                 ;; Parse the current line. Mimics how
                 ;; move-past-close-and-reindent does it.
                 (parse-state
                  (parse-partial-sexp pos-of-indent
                                      end-of-line
                                      nil nil
                                      `(0 nil nil
                                          ,(null (calculate-lisp-indent))
                                          nil nil nil nil nil)))
                 (comment-start (nth 8 parse-state)))
            (when comment-start (goto-char comment-start))
            ;; Navigate to right after last sexp
            (backward-sexp)
            (forward-sexp)
            ;; TODO: Generalize by saving off what was deleted and inserting it here
            (insert ")")))))))

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
(defun backward-indent-adjust-sexp (&optional prefix-arg)
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

;; TODO: Account for this case. Ensure:
;;   - Take the outer close paren of (x (some-func 10)), not from within comments
;;   - Reindent the ;; comment too
;; | indicates point.
  (defun func ()
    (let ((x (some-func 10)) ; (First comment)
          ;; (Second comment)
          |(y (some-func 20)))))
;; After 2 TAB :
  (defun func ()
    (let ((x (some-func 10 ; (First comment)
                        ;; (Second comment)
                        |)) (y (some-func 20)))))

