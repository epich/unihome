;;to set foreground color to white
(set-foreground-color "white")
;;to set background color to black
(set-background-color "black")

;; General emacs settings
(setq visible-bell t) 
(tool-bar-mode 0)
(column-number-mode 1)

;; Disable the auto-save because I don't want to be undermined about what and when to save.
(setq auto-save-default nil)

;; Maximize window upon startup.
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)
(toggle-fullscreen)

(setq case-replace nil)

;; make file name and computer title
(setq-default
 frame-title-format
 '(:eval
   (format "%s"
           (file-name-nondirectory (or (buffer-file-name) default-directory))))) 

(custom-set-variables
 '(evil-overriding-maps nil)
)

;;; Functions to facilitate elisp debug logging.
(defvar current-date-time-format "%Y-%m-%dT%H:%M:%S"
  "Format for date string. ")
(defun get-usec-str (cur-time)
   "Get the microseconds as string. "
   (format "%06d"
      (nth 2 cur-time
      )
   )
)
(defun get-time-str ()
   "Get the current time as a string. "
   (interactive)
   (let ((cur-time (current-time)))
      (format "%s.%s" 
         (format-time-string current-date-time-format)
         (get-usec-str cur-time)
      )
   )
)
(defun log-msg (msg)
   "Log a message, with prepended information.  Used for debugging. "
   (interactive)
   (message (format "%s %s"
      (get-time-str) msg
   ))
)

;; Initialize evil
(log-msg "Initializing Evil.")
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(log-msg "Finished initializing Evil.")

;; Initialize cc-mode
;;
;; cc-mode is distributed with Emacs, but using a newer version in order to get 
;; style guessing.
; c-guess isn't working out.
;(add-to-list 'load-path "~/.emacs.d/cc-mode")

; evil-integration.el attempts to recreate the evil-overriding-maps, set
; that code to nil to prevent it from running.
(eval-after-load 'ibuffer nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(evil-search-module (quote evil-search))
 '(inhibit-startup-screen t))
 '(evil-overriding-maps nil)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;; Tabs
; Make tab less restrictive about where I tab to.
(global-set-key (kbd "TAB") 'tab-to-tab-stop);
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
; Permanently force Emacs to indent with spaces, never with TABs:
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (cdr (number-sequence 0 256 3)))
(setq c-basic-offset 3)
;(setq tab-width 4)

;;; evil mappings
;;;

(define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
(define-key evil-motion-state-map (kbd "C-c") 'evil-normal-state)
(define-key isearch-mode-map (kbd "C-c") 
   (lambda ()
      (interactive)
      (dotimes (num 2) (isearch-abort))
   )
)
; I don't use RET in motion state, but it is useful in eg buffer mode.
(global-set-key (kbd "RET") 'evil-ret)
(define-key evil-motion-state-map (kbd "RET") nil)
(define-key evil-normal-state-map "o" nil)
(define-key evil-normal-state-map "O" nil)
(define-key evil-motion-state-map "o" 'next-buffer)
(define-key evil-motion-state-map "O" 'previous-buffer)
(define-key evil-normal-state-map "f" 'buffer-menu)
(define-key evil-normal-state-map "-" nil)
(define-key evil-motion-state-map "-" 'evil-end-of-line)
(define-key evil-normal-state-map "s" nil)
(define-key evil-motion-state-map "sf" 'delete-other-windows)
(define-key evil-motion-state-map "sh" 'highlight-phrase)
(define-key evil-normal-state-map ";" nil)
(define-key evil-motion-state-map "se" 'eval-last-sexp)
(when (fboundp 'undo-tree-undo)
   (define-key evil-normal-state-map "U" 'undo-tree-redo))
;; Go down in larger steps
(define-key evil-motion-state-map ";" 
   (lambda ()
      (interactive)
      (dotimes (num 8)
         (scroll-up 1)
         (evil-next-line)
      )
      ; Update highlights.  Without this, I've observed it 
      ; scrolls fast enough to miss highlighting search terms.
      (evil-ex-hl-update-highlights)
   )
)
(define-key evil-normal-state-map "'" nil)
;; Go up in larger steps
(define-key evil-motion-state-map "'" 
   (lambda ()
      (interactive)
      (dotimes (num 8) 
         (scroll-down 1)
         (evil-previous-line)
      )
      ; Update highlights.  Without this, I've observed it 
      ; scrolls fast enough to miss highlighting search terms.
      (evil-ex-hl-update-highlights)
   )
)

(defun scroll-down-keep-cursor ()
   ;; Scroll the text one line down while keeping the cursor
   (interactive)
   (scroll-down 1))

(defun scroll-up-keep-cursor ()
   ;; Scroll the text one line up while keeping the cursor
   (interactive)
   (scroll-up 1)) 

;; Mappings to Emacs Meta and Ctrl key bindings.
(define-key evil-normal-state-map "," 'execute-extended-command)
(define-key evil-normal-state-map "c" nil)

;(setq truncate-lines nil)

;; Change color of isearch lazy highlighting
;;
;; Thanks to: http://lists.gnu.org/archive/html/help-gnu-emacs/2003-03/msg00108.html
(defun configure-faces (fl) "Set face attributes and create faces when 
necessary"
  (mapc (lambda (f)
          (unless (boundp (car f)) (make-empty-face (car f)))
          (eval `(set-face-attribute (car f) nil ,@(cdr f))))
        fl))
(configure-faces
   '((isearch-lazy-highlight-face' :background "yellow" :foreground "black"))
)

(defun my-update-style ()
   "Update style for current buffer. " 
   (interactive)
   ;(when (boundp 'c-guess)
   ;   (log-msg "Invoking c-guess")
   ;    TODO: Doesn't actually set c-basic-offset .  When done via M-x, takes too long.
   ;   (c-guess)
   ;   (log-msg "Finished invoking c-guess")
   ;)
)

(defun my-c-mode-common-hook ()
   (log-msg "Settup up C mode. ")
   (my-update-style)
)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-hook 'after-change-major-mode-hook
   (lambda ()
      ;; Force Evil mode in Fundamental mode.
      (evil-mode 1)
))

(add-hook 'term-setup-hook
   (lambda ()
      (define-key evil-motion-state-map "ch" help-map)
      (define-key evil-motion-state-map "cx" ctl-x-map)
      (delete-other-windows)
      ;(setq search-whitespace-regexp nil)
      (log-msg "Finished with term-setup-hook. ")
   )
)

(log-msg "Finished .emacs file. ")

