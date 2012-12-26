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


(defun get-region-boundaries (beg end)
  "Log the beg and end of the current region. "
  (interactive "r")
  (log-msg "DEBUG: beg=%s end=%s" beg end))

;; This is test text
;; (alpha, beta, gamma, delta, epsilon) blah (11, 22, 33, 44, 55)


# 1023,
# 1023,
(setq match-list nil)
(defun make-match-list-backwards (regex beg end)
  "Return a list of REGEX matches in the region from END to BEG. "
  (let ((match-list nil))
    (save-excursion
      (goto-char end)
      (while (re-search-backward regex beg t)
        (setq match-list (cons (match-string 0) match-list)))
      match-list)))




(progn
  (log-msg "DEBUG: match-list forwards : %s" (make-match-list "\\([a-zA-Z]+\\)," 5539 5626))
  (log-msg "DEBUG: match-list backwards: %s" (make-match-list-backwards "\\([a-zA-Z]+\\)," 5539 5626))
  (set-match-list "\\([a-zA-Z]+\\)," 5539 5626)
  (log-msg "DEBUG: match-list=%s" match-list)
  (log-msg "DEBUG: cycle-match-list 1st: %s" (cycle-match-list "foo")) 
  (log-msg "DEBUG: cycle-match-list 2nd: %s" (cycle-match-list "foo")) 
  (log-msg "DEBUG: cycle-match-list 3rd: %s" (cycle-match-list "foo")) 
  (log-msg "DEBUG: cycle-match-list 4th: %s" (cycle-match-list "foo")) 
  (log-msg "DEBUG: cycle-match-list 5th: %s" (cycle-match-list "foo")) 
  (log-msg "DEBUG: cycle-match-list 6th: %s" (cycle-match-list "foo")) 
)

(progn
  (setq my-list '(1 2 3))
  (setq my-list (nreverse my-list))
  my-list)
