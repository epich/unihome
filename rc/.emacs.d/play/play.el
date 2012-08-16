;;; General purpose elisp, which should persist more than a scratch buffer.

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

