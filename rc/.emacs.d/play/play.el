;;; General purpose elisp, which should persist more than a scratch buffer.

;; Using a named function to map C-e to C-c, the Key Translation is successful:
  (progn (fset 'foo (lambda (prompt) (kbd "C-c")))
         (define-key key-translation-map (kbd "C-e") 'foo))
        
;; Using an anonymous function for another keymap binds C-e to the lambda-defined command.
  (define-key global-map
              (kbd "C-e")
              (lambda () (interactive) (message "Inside C-e's lambda")))
           
;; But using an anonymous function for key-translation-map does not change behavior of inputting C-e:
  (define-key key-translation-map
              (kbd "C-e")
              (lambda (prompt) (kbd "C-c")))

