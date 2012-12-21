;;; General purpose elisp, which should persist more than a scratch buffer.
;;
;; Using a named function to map C-e to C-c, the Key Translation is successful:
;;
(setq lexical-binding t)

  (progn (fset 'foo (lambda (prompt) (kbd "C-c")))
         (define-key key-translation-map (kbd "C-e") 'foo)
         )

(functionp 'foo)

(progn
  (setq bar 4)
  (functionp bar)
  )

;; Using an anonymous function for another keymap binds C-e to the lambda-defined command.
;;
  (define-key global-map
              (kbd "C-e")
              (lambda () (interactive) (message "Inside C-e's lambda")))
           
;; But using an anonymous function for key-translation-map does not change behavior of inputting C-e:
;;
  (define-key key-translation-map
              (kbd "C-e")
              (lambda (prompt) (kbd "C-c")))



(progn
 (defun demo-lex-a ()
   (setq x 20)
   )
 (defun demo-lex-b (x)
   (demo-lex-a)
   (message "x is %d" x)
   )
 (demo-lex-b 10)
 )

;; Demonstrates with closures
(setq lexical-binding t)
(defun make-adder (base) (lambda (num) (+ num base)))
(setq x 1)
(setq 1-adder (make-adder x))
(funcall 1-adder 10)
(funcall 1-adder 100)
(setq x 100)
(funcall 1-adder 100)

;; Demonstrates without closures
(defun make-adder (base) (lambda (num) (+ num base)))
(setq x 1)
(setq 1-adder (make-adder x))
(funcall 1-adder 10)





(setq lexical-binding t)
(defun make-conditional-key-translation (key-from key-to translate-keys-p)
  "Make a Key Translation such that if the translate-keys-p function returns true,
key-from translates to key-to, else key-from translates to itself.  translate-keys-p
takes no args.

lexical-binding must be t in order for this to work correctly. "
  (log-msg (format "DEBUG: Inside make-conditional-key-translation")) 
  (define-key key-translation-map key-from
              (lambda (prompt)
                      (log-msg (format "DEBUG: Inside closure")) 
                      (if (funcall translate-keys-p) key-to key-from)))
  )
(defun my-translate-keys-p ()
  "Returns whether conditional key translations should be active.  See make-conditional-key-translation function. "
  (log-msg (format "DEBUG: Inside my-translate-keys-p")) 
  (or (evil-motion-state-p) (evil-normal-state-p) (evil-visual-state-p))
  )
;; TODO: Doesn't work yet
(make-conditional-key-translation (kbd "ce") (kbd "C-e") 'my-translate-keys-p)
(message "key-translation-map: key:%s %s" (kbd "t") key-translation-map)

(define-key key-translation-map (kbd "cf") (lambda (prompt) (kbd "C-e")))




;; Demonstrates taking an existing function definition and adding a line of code to it.
;; Kind of like function advising.
(defun foo ()
   (message "Inside foo")
   )
(symbol-function 'foo)
(fset 'foo (append (symbol-function 'foo) '((message "Additional words"))))
(foo)
(print (symbol-function 'foo))
(byte-compile 'foo)
(disassemble 'foo)
(disassemble 'evil-ex-search-next)

(setq myf (lambda () (message "Inside lambda")))
(setq myf (append myf '((message "Additional words"))))
(funcall (eval myf))

(let ((list '(1 2 3)) (new-elem 4))
  (setq list (cons new-elem list)))

(defun make-match-list (regex matches-var-name beg end)
  "Make a list of REGEX matches in the region from BEG to END and assign to a variable named by the MATCHES-VAR-NAME string. "
  (interactive "sRegex: \nsVariable to store matches: \nr")
  (let ((matches-sym (intern matches-var-name)))
    (set matches-sym nil)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regex end t)
        (set matches-sym (cons (match-string 0) (eval matches-sym)))))))


(defvar last-match-list nil
  "A symbol for the last saved match list from calling the save-match-list function. ")
(defun make-match-list (regex beg end)
  "Return a list of REGEX matches in the region from BEG to END. "
  (let ((match-list nil))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regex end t)
        (setq match-list (cons (match-string 0) match-list)))
      match-list)))
(defun save-match-list (regex matches-var-name beg end)
  "Make a list of REGEX matches in the region from BEG to END and assign to a variable named by the MATCHES-VAR-NAME string. "
  (interactive "sRegex: \nsVariable to store matches: \nr")
  (let ((matches-sym (intern matches-var-name)))
    (set matches-sym (make-match-list regex beg end))
    (setq last-match-list matches-sym)))
gogogo
(log-msg "DEBUG: mym=%s last-match-list=%s" mym last-match-list)
(stringp (car mym))
(symbolp last-match-list)

(replace-match "xx")

: distribute-and-replace function:
   : Inputs:
      : region
      : regex
      : symbol (offer a default)
   : Algorithm:
      : Function replaces all matches to regex in region with elements from the list (variable cell of symbol)


;; TODO: Verify right values passed to replace-match
(defun distribute-replace-from-list-var (regex dist-list beg end)
  "TODO"
  (let ((list-i dist-list))
    (save-excursion
      (goto-char beg)
      (while (and list-i (re-search-forward regex end t))
        (replace-match (car list-i))
        (setq list-i (cdr list-i))))))
(defun distribute-replace-from-list (regex list-var-name beg end)
  "TODO"
  (interactive "sRegex: \nsVariable containing list: \nr") ; TODO set default
  (let ((matches-sym (intern matches-var-name)))
    (set matches-sym (make-match-list regex beg end))
    (setq last-match-list matches-sym)))




(defvar match-list nil
  "A list of matches, as set through the set-match-list and consumed by the cycle-match-list function. ")
(defvar match-list-iter nil
  "Iterator through the global match-list variable. ")
(defun reset-match-list-iter ()
  "Set match-list-iter to the beginning of match-list. "
  (interactive)
  (setq match-list-iter match-list))
(defun make-match-list (regex beg end)
  "Return a list of REGEX matches in the region from BEG to END. "
  (let ((match-list nil))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regex end t)
        (setq match-list (cons (match-string 0) match-list)))
      match-list)))
(defun set-match-list (regex beg end)
  "Make a list of REGEX matches in the region from BEG to END and assign to the global match-list variable. "
  (interactive "sRegex: \nr")
  (setq match-list (make-match-list regex beg end))
  (reset-match-list-iter))
(defun cycle-match-list (&optional after-end-string)
  "Return the next element of match-list.

If after-end-string is nil, cycle back to the beginning of match-list.
Else return after-end-string once the end of match-list is reached."
  (let ((next-elm (car match-list-iter)))
    (setq match-list-iter (cdr match-list-iter))
    (if next-elm
        next-elm
      (if after-end-string
          after-end-string
        (reset-match-list-iter)))))


