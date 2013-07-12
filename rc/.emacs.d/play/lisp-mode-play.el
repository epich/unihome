;; Goals:
;;   - Make it so as programmer doesn't need to pay attention to the
;;     close parens whose opening is not on the same line. Moves close
;;     parens around accordingly.
;;   - Provide an editing experience more like Python.

;; If there is non whitespace before point on the same line:
;;   Resort to whatever TAB would normally do instead (indent-for-tab-command ?)
;; Else if the end of previous line does not end with a close paren:
;;   Resort to whatever TAB would normally do instead (indent-for-tab-command ?)
;; Else:
;;   - Delete last close paren and move it after point
;;   - If the char after point is not one of:
;;       ) ] } whitespace end-of-line
;;     then put a space between the close paren and it
;;     TODO: Look at insert-parentheses to see what it does for inserting space
;;   - Indent (indent-for-tab-command ?)
(defun forward-and-adjust-sexp (&optional prefix-arg)
  (interactive "P")
  )

;; If there is non whitespace before point on the same line:
;;   Resort to whatever DEL would normally do instead
;; Else if at the beginning of line already:
;;   Resort to whatever DEL would normally do instead
;; Else find next close paren on same line as point whose open paren is on a previous line
;;   - Delete close paren and move to close same sexp on previous line
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
               |(format "foo") "bar") (progn)))
;; Possibility 1: violates invariant, see NB
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func"))
      (format "foo" "bar") (progn))) ; NB: Violates principle of only deleting close parens whose opening is not on the same line
;; Possibility 2: leads to misleading formatting, see NB
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func"))
      (format "foo") "bar") (progn)) ; NB: (progn) is outside of let, but formatting suggests otherwise
;; Possibility 3: Do this
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func"))
      (format "foo") "bar" (progn))) ; NB: Take the close paren that matches to the first open paren of the previous line

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

;; TODO: Account for this case. Specifically:
;;   - Take the close paren of (x 10), not from (My comment)
;;   - Reindent the comment too
;; | indicates point.
  (defun func ()
    (let ((x (some-func 10))
          ;; (My comment)
          |(y (some-func 20)))))
;; After 2 TAB :
  (defun func ()
    (let ((x (some-func 10
                        ;; (My comment)
                        |)) (y (some-func 20)))))

