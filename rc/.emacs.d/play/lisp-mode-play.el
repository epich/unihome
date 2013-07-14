;; Goals:
;;   - Make it so as programmer doesn't need to pay attention to the
;;     close parens whose opening is not on the same line. Moves close
;;     parens around accordingly.
;;   - Provide an editing experience more like Python.

;; If there is non whitespace before point on the same line (see back-to-indentation):
;;   Resort to whatever TAB would normally do instead (indent-for-tab-command ?)
;; Else if the end of previous line does not end with a close paren:
;;   Resort to whatever TAB would normally do instead (indent-for-tab-command ?)
;; Else:
;;   - Delete last close paren and move it after point
;;     - Find close paren:
;;       - Go to beginning of next line so as not in a comment
;;       - backward-list forward-list
;;   - If the char after point is not one of:
;;       ) ] } whitespace end-of-line
;;     then put a space between the close paren and it
;;     TODO: Look at insert-parentheses to see what it does for inserting space
;;   - Indent (indent-for-tab-command ?)
(defun forward-and-adjust-sexp (&optional prefix-arg)
  (interactive "P")
  )

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
(defun backward-and-adjust-sexp (&optional prefix-arg)
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

