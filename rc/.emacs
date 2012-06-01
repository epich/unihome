;; General emacs settings
(setq visible-bell t) 
(tool-bar-mode 0)
(column-number-mode 1)

;; Disable the auto-save, the #* debris files slow down Emacs startup.
(setq auto-save-default nil)
(global-auto-revert-mode 1)
(defun my-window-scroll
  (log-msg "Inside my-window-scroll")
)
; TODO
;(add-hook 'window-scroll-functions 'my-window-scroll nil t)

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
(setq vc-follow-symlinks t)
(delete-selection-mode 1)
(setq mouse-yank-at-point t)

;; make file name and computer title
(defvar my-frame-title "Unset" "Title for the frame. ")
(setq-default
 frame-title-format
 '(:eval
   (format "%s"
           (file-name-nondirectory (or (buffer-file-name) default-directory))))) 

;; Customizations that have to come before Evil.
; In custom-set-variables , would be: '(evil-overriding-maps nil)
(setq evil-overriding-maps nil)
(setq revert-without-query (quote (".*")))

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

; TODO:
;  : Use advice getters and setters instead of setq
;  : Set activation flag?
;  : Byte compile
;  : Preactivate?
;  : Use ad-define-subr-args because message is primitive
;(defadvice message (before message-add-prefix-advice
;                           (format-string &rest args))
;  "Add prefix string to message."
;  (let ((format-string-pos 0) (args-pos 1))
;    (setq format-string (concat "%s" format-string))
;    (setq args (cons "===BJO=== " args))
;    ))

(add-to-list 'load-path "~/.emacs.d")
;; Compile .el files if they need to be.
;;
;; From: http://stackoverflow.com/questions/1217180/how-do-i-byte-compile-everything-in-my-emacs-d-directory
; TODO: When files don't compile, it'll create errors and modest delay everytime Emacs starts.
;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

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

;; Initialize Clearcase extensions
; Initializes too slowly for my tastes, even when .elc exists.
; Specifically, I observed an additional 12 seconds for 17 script files in a snapshot view.
;(load "clearcase")

;; Initialize Auto Complete
;;
(defvar my-ac-build-path "~/.emacs.d/ac/auto-complete-1.3.1/build" "Path to built ac")
(add-to-list 'load-path my-ac-build-path)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (format "%s/ac-dict" my-ac-build-path))
(ac-config-default)

;; Configure Auto Complete
; RET can cause auto completion when literal RET is what I want.
; Auto Complete is perfectly usable via TAB alone, so disable RET key binding.
(define-key ac-complete-mode-map (kbd "RET") nil)

;; Initialize Yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)

;; Initialize Java Auto Complete
(add-to-list 'load-path "~/.emacs.d/ajc-java-complete/")
(require 'ajc-java-complete-config)
(add-hook 'java-mode-hook 'ajc-java-complete-mode)
(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

; evil-integration.el attempts to recreate the evil-overriding-maps, set
; that code to nil to prevent it from running.
(eval-after-load 'ibuffer nil)

;;; Tabs
(defvar my-offset 3 "My size of indentation.  Would prefer style guessing instead. ")
(defun my-continuation-offset ()
  "Determine the offset for line continuations."
  (* 3 my-offset))
; Make tab less restrictive about where I tab to.
(global-set-key (kbd "TAB") 'tab-to-tab-stop);
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
; Permanently force Emacs to indent with spaces, never with TABs:
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (cdr (number-sequence 0 256 my-offset)))
(setq c-basic-offset my-offset)
; Doesn't work here, works in custom-set-variables.
;(setq evil-shift-width my-offset)
; Determines how to display tabs.
;(setq tab-width my-offset)
; Disable weird auto formatting
(setq-default c-electric-flag nil)

;; ac-delay
; The default 0.1 ac-delay can cause display update delays when I'm typing.
; If I know what I'm typing, it is inconvenient.  1.0 is sufficiently high
; to imply I'm pausing in my typing.
;; inverse-video
; An attempt to get white on black.  For some reason this doesn't work
; but the --reverse-video CLI arg does.
;; x-select-enable-clipboard
; This is necessary to paste into Windows running on qemu-kvm .
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ac-delay 1.0)
 '(ajc-tag-file "~/.emacs.d/ajc-java-complete/java_base.tag")
 '(evil-overriding-maps nil)
 '(evil-search-module (quote evil-search))
 '(evil-shift-width my-offset)
 '(inhibit-startup-screen t)
 '(inverse-video t)
 '(large-file-warning-threshold 100000000)
 '(python-continuation-offset (my-continuation-offset))
 '(x-select-enable-clipboard t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;; evil key bindings
;;;
;;; Since normal state inherits motion state's key bindings,
;;; If I want to override a default binding in normal state 
;;; with one in motion state, I have to expressly set the old
;;; key binding to nil.

(defun my-esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ((or (evil-insert-state-p) (evil-motion-state-p)) [escape])
   ; This is the best way I could infer for now to have C-c work during evil-read-key.
   ((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
   (t (kbd "C-g")))
)
(define-key key-translation-map (kbd "C-c") 'my-esc)

(define-key evil-motion-state-map "," 'execute-extended-command)
; Undo c Evil keybinding for use as prefix key to various Ctrl- key sequences.
(define-key evil-normal-state-map "c" nil)

;; Disable C-0 and C-- since I hit them alot.
(defun my-no-op () "No op, meaning do nothing."
  )
(define-key evil-motion-state-map (kbd "C-0") 'my-no-op)
(define-key evil-normal-state-map (kbd "C-0") 'my-no-op)
(define-key evil-motion-state-map (kbd "C--") 'my-no-op)
(define-key evil-normal-state-map (kbd "C--") 'my-no-op)

; I don't use RET in motion state, but it is useful in eg buffer mode.
(define-key evil-motion-state-map (kbd "RET") nil)
(global-set-key (kbd "RET") 'evil-ret)
; Will use Emacs C-y for paste rather than Evil's evil-scroll-line-up.
(define-key evil-motion-state-map (kbd "C-y") nil)
(define-key evil-normal-state-map "o" nil)
(define-key evil-motion-state-map "o" 'next-buffer)
(define-key evil-normal-state-map "O" nil)
(define-key evil-motion-state-map "O" 'previous-buffer)
(define-key evil-motion-state-map "f" 'buffer-menu)
(define-key evil-normal-state-map "-" nil)
(define-key evil-motion-state-map "-" 'evil-end-of-line)
(define-key evil-normal-state-map "s" nil)
(define-key evil-motion-state-map "sf" 'delete-other-windows)
(define-key evil-motion-state-map "sh" 'highlight-phrase)
(define-key evil-motion-state-map "se" 'eval-last-sexp)
(define-key evil-motion-state-map "srb" 'revert-buffer)
(define-key evil-motion-state-map "sle" (lambda () (interactive) (load-file "~/.emacs") (toggle-fullscreen)))
(define-key evil-motion-state-map "sji" 'ajc-import-class-under-point)
; Use U for redo.  This is meant to mimic a similar line in evil-maps.el .
(when (fboundp 'undo-tree-undo)
   (define-key evil-normal-state-map "U" 'undo-tree-redo))

(define-key evil-normal-state-map ";" nil)
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

;(setq truncate-lines nil)

;; Change color of isearch lazy highlighting
;;
;; Thanks to: http://lists.gnu.org/archive/html/help-gnu-emacs/2003-03/msg00108.html
(defun configure-faces (fl)
  "Set face attributes and create faces when necessary"
  (mapc (lambda (f)
          (unless (boundp (car f)) (make-empty-face (car f)))
          (eval `(set-face-attribute (car f) nil ,@(cdr f))))
        fl))
(configure-faces
 '((isearch-lazy-highlight-face' :background "yellow" :foreground "black"))
 )

(defun my-insert-elisp-log ()
   "Insert log statement for elisp. "
   (interactive)
   (insert "(log-msg (format \"DEBUG: \")) ; TODO: temporary for debug")
   (search-backward "DEBUG: ")
   (goto-char (match-end 0))
)
(defun my-insert-java-log ()
   "Insert log statement for Java. "
   (interactive)
   ; The vimscript was:
   ;imap <F3> org.slf4j.LoggerFactory.getLogger(this.getClass()).warn( // temporary for debug<Enter><Tab><Tab><Tab>"DEBUG: ",<Enter>new Object[]{} );<Esc>khi
   (insert "org.slf4j.LoggerFactory.getLogger(this.getClass()).warn( // TODO: temporary for debug\n\t\t\t\"DEBUG: \",\n\t\t\tnew Object[]{} );")
   (search-backward "DEBUG: ")
   (goto-char (match-end 0))
)

(add-hook 'emacs-lisp-mode-hook 
   (lambda ()
      (log-msg "Inside emacs-lisp-mode-hook")
      (define-key evil-insert-state-local-map (quote [f3]) 'my-insert-elisp-log)
   )
)
(add-hook 'java-mode-hook 
   (lambda ()
      (log-msg "Inside java-mode-hook")
      (define-key evil-insert-state-local-map (quote [f3]) 'my-insert-java-log)
   )
)
(add-hook 'c-mode-common-hook 
   (lambda ()
      (log-msg "Inside c-mode-common-hook. ")
   )
)
(add-hook 'after-change-major-mode-hook
   (lambda ()
      ;; Force Evil mode in Fundamental mode.
      (evil-mode 1)
))
(add-hook 'term-setup-hook
   (lambda ()
      ;; I tend to put things here that for some reason don't work
      ;; when executed earlier.
     
      (define-key evil-motion-state-map "ch" help-map)
      (define-key evil-motion-state-map "cx" ctl-x-map)
      (delete-other-windows)
      ;(setq search-whitespace-regexp nil)
      (log-msg "Finished with term-setup-hook. ")
   )
)

(log-msg "Finished .emacs file. ")

