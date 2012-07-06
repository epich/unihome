;;; General emacs settings
(setq visible-bell t) 
(tool-bar-mode 0)
(column-number-mode 1)
;; Disable the auto-save, the #* debris files slow down Emacs startup.
(setq auto-save-default nil)
(global-auto-revert-mode 1)
(setq revert-without-query (quote (".*")))
(setq case-replace nil)
(setq vc-follow-symlinks t)
(delete-selection-mode 1)
(setq mouse-yank-at-point t)
(show-paren-mode 1)
(setq show-paren-delay 0)
;; Emacs stupidly formats curly braces in neither of the two most common ways.
;; This fixes that.
(setq c-default-style "linux")
;; Font size (multiplied by 10)
(set-face-attribute 'default nil :height 80)
;;(setq truncate-lines nil)

;;; Version specific elisp
;; electric-pair-mode introduced in version 24.
(cond ((<= 24 emacs-major-version)
       (electric-pair-mode 1)))

;; Maximize window upon startup.  A non toggling way to do this would be nice.
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
(toggle-fullscreen)

;; Set the frame title to the current filename.
(setq-default frame-title-format
              '(:eval (format "%s"
                              (file-name-nondirectory (or (buffer-file-name) default-directory)))))

(defun my-no-op () "No op, meaning do nothing.")

;;; Functions to facilitate elisp debug logging.
(defvar current-date-time-format "%Y-%m-%dT%H:%M:%S"
  "Format for date string. ")
(defun get-usec-str (cur-time)
   "Get the microseconds as string. "
   (format "%06d"
      (nth 2 cur-time)))
(defun get-time-str ()
   "Get the current time as a string. "
   (interactive)
   (let ((cur-time (current-time)))
      (format "%s.%s" 
         (format-time-string current-date-time-format)
         (get-usec-str cur-time))))
(defun log-msg (msg)
   "Log a message, with prepended information.  Used for debugging.

I attempted to use defadvice on the message function, but the minibuffer
misbehaves under some conditions.  The message function is a C primitive
anyway, which doesn't always combine with defadvice. "
   (interactive)
   (message (format "%s %s" (get-time-str) msg)))

;; (add-to-list 'load-path "~/.emacs.d")
;; Compile .el files if they need to be.
;;
;; From: http://stackoverflow.com/questions/1217180/how-do-i-byte-compile-everything-in-my-emacs-d-directory
;; TODO: When files don't compile, it'll create errors and modest delay everytime Emacs starts.
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;;; Initialize evil
(log-msg "Initializing Evil.")
;; Before initializing Evil.  In custom-set-variables , would be: '(evil-overriding-maps nil)
(setq evil-overriding-maps nil)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
;; evil-integration.el attempts to recreate the evil-overriding-maps, set
;; that code to nil to prevent it from running.
(eval-after-load 'ibuffer nil)

;;; Initialize Clearcase extensions
;;
;; Initializes too slowly for my tastes, even when .elc exists.
;; Specifically, I observed an additional 12 seconds for 17 script files in a snapshot view.
;;
;; Might still be useful for merging. Investigate.
;; (load "clearcase")

;;; Initialize Auto Complete
(defvar my-ac-build-path "~/.emacs.d/auto-complete-1.3.1/build" "Path to built ac")
(defun setup-auto-complete ()
  "Set up Auto Complete"
   ;; Initialize 
   (log-msg "Initializing Auto Complete.")
   (add-to-list 'load-path my-ac-build-path)
   (require 'auto-complete-config)
   (add-to-list 'ac-dictionary-directories (format "%s/ac-dict" my-ac-build-path))
   (ac-config-default)

   ;; Configure
   ;;
   ;; RET can cause auto completion when literal RET is what I want.
   ;; Auto Complete is perfectly usable via TAB alone, so disable RET key binding.
   (define-key ac-complete-mode-map (kbd "RET") nil))
;; I'm not finding Auto Complete useful at the moment.
;; (init-auto-complete)

;;; Initialize CEDET
(log-msg "Initializing CEDET.")
(defvar my-cedet-path "~/.emacs.d/cedet-1.1" "Path to CEDET")
(add-to-list 'load-path (format "%s/common" my-cedet-path))
;; CEDET retardedly raises fatal error when reloading an already reloaded file,
;; undermining reloading of my .emacs file.  This hacks that fix.
(ignore-errors (load-file (format "%s/common/cedet.el" my-cedet-path)))

;;; Initialize JDEE
(defvar my-jdee-path "~/.emacs.d/jdee-2.4.0.1" "Path to JDEE")
(defun setup-jdee ()
  "Set up JDEE"
   (log-msg "Initializing JDEE.")
   (add-to-list 'load-path (format "%s/lisp" my-jdee-path))
   ;; NB: (require 'jde) in Java mode hook, so as startup is more
   ;; efficient when not editing Java.
   ;;(require 'jde)
   
   ;; Online posting says these might be necessary for JDEE.
   ;; http://forums.fedoraforum.org/showthread.php?t=280711
   ;; (defun screen-width nil -1)
   ;; (define-obsolete-function-alias 'make-local-hook 'ignore "21.1")

   ;; Docs indicate elib is a dependency.  However, I haven't witnessed
   ;; a problem yet.
   )
(setup-jdee)

;;; Initialize ParEdit
(log-msg "Initializing Paredit.")
(add-to-list 'load-path "~/.emacs.d/paredit")
(require 'paredit)
;; NB: I don't enable-paredit-mode because it is not designed to support non wholistic
;; editing.  It intends the user to add and remove parens always in pairs.  Inserting
;; only a closing paren in a valid place doesn't work well, I would need to use C-q to
;; do so.  Another example is commenting out code.  ParEdit expects you would use the
;; wholistic M-; and will misbehave if you add ';' characters line by line.
;;
;; Since I like occasional non wholistic editing, I use ParEdit functions without
;; the minor mode enabled.

;; Initialize project-specific elisp
(log-msg "Initializing project-specific elisp.")
;; GOESR isn't relevant to all computers I work on, so ignore errors.
(ignore-errors (load-file "~/goesr/goesrDev.el"))
;; Classpaths for JDEE
(ignore-errors (defvar my-java-classpath goesr-classpath "Path for my .class or .jar files.")
   (defvar my-java-sourcepath goesr-sourcepath "Path for my .java files."))

;; This is the patched delete-trailing-whitespace posted to
;;  http://lists.gnu.org/archive/html/emacs-devel/2011-02/msg00523.html
;; and accepted into Emacs.  It's not in my version, so just copying it here
;; for use with = key binding.
(defun patched-delete-trailing-whitespace (&optional start end)
  "Delete all the trailing whitespace across the current buffer.
All whitespace after the last non-whitespace character in a line is deleted.
This respects narrowing, created by \\[narrow-to-region] and friends.
A formfeed is not considered whitespace by this function.
If the region is active, only delete whitespace within the region."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (save-match-data
    (save-excursion
      (let ((end-marker (copy-marker (or end (point-max))))
            (start (or start (point-min))))
        (goto-char start)
        (while (re-search-forward "\\s-$" end-marker t)
          (skip-syntax-backward "-" (save-excursion (forward-line 0) (point)))
          ;; Don't delete formfeeds, even if they are considered whitespace.
          (save-match-data
            (if (looking-at ".*\f")
                (goto-char (match-end 0))))
          (delete-region (point) (match-end 0)))
        (set-marker end-marker nil)))))

;;; Relating to tabs
;; I would prefer automatic guessing of my-offset based on the offset in use for the
;; surrounding code.
(defvar my-offset 3 "My indentation offset. ")
(defun my-continuation-offset ()
  "Determine the offset for line continuations."
  (* 3 my-offset))
;; For binding to backspace.
;;
;; Taken from: http://stackoverflow.com/questions/1450169/how-do-i-emulate-vims-softtabstop-in-emacs
;;
;; Doesn't correctly handle backspace when there's a selection.
;; Doesn't correctly handle edge case near beginning of buffer.
(defun backward-delete-whitespace-to-tab-stop ()
  "Delete back to the previous tab-stop of whitespace, or as much whitespace as possible,
or just one char if that's not possible"
  (interactive)
  (if indent-tabs-mode
      (call-interactively 'backward-delete-char)
    (let ((movement (% (current-column) my-offset))
          (p (point)))
      (when (= movement 0) (setq movement my-offset))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))
;; Make tab less restrictive about where I tab to.
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-insert-state-map (kbd "DEL") 'backward-delete-whitespace-to-tab-stop)
;; Permanently force Emacs to indent with spaces, never with TABs:
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (cdr (number-sequence 0 256 my-offset)))
(setq c-basic-offset my-offset)
;; Doesn't work here, works in custom-set-variables.
;;(setq evil-shift-width my-offset)
;; Determines how to display tabs.
;;
;; It's best to use the default, for Python editing and because some Emacs elisp code is formatted on that assumption.
;;(setq tab-width my-offset)
;; Disable weird auto formatting
(setq-default c-electric-flag nil)

;;; Customizations
;;
;; Specific customizations are documented outside the sexp, because
;; Emacs deletes all comments within.
;;
;; Also slightly annoying is that when changing this sexp, Emacs will
;; change numbers in eg 1e9 notation, even though 1e9 is more readable
;; than 1000000000.  Lame.
;;
;; ac-delay
;;    The default 0.1 ac-delay can cause display update delays when I'm typing.
;;    If I know what I'm typing, it is inconvenient.  1.0 is sufficiently high
;;    to imply I'm pausing in my typing.
;; inverse-video
;;    An attempt to get white on black.  For some reason this doesn't work
;;    but the --reverse-video CLI arg does.
;; x-select-enable-clipboard
;;    This is necessary to paste into Windows running on qemu-kvm .
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 1.0)
 '(evil-overriding-maps nil)
 '(evil-search-module (quote evil-search))
 '(evil-shift-width my-offset)
 '(global-whitespace-mode t)
 '(inhibit-startup-screen t)
 '(inverse-video t)
 '(jde-global-classpath my-java-classpath)
 '(jde-jdk-registry (quote (("1.6.0" . "/usr/lib/jvm/java-1.6.0-openjdk.x86_64"))))
 '(jde-sourcepath my-java-sourcepath)
 '(large-file-warning-threshold 1000000000.0)
 '(message-log-max 100000)
 '(nxml-attribute-indent (my-continuation-offset))
 '(nxml-child-indent my-offset)
 '(python-continuation-offset (my-continuation-offset))
 '(python-indent my-offset)
 '(scroll-conservatively 1)
 '(scroll-step 1)
 '(whitespace-style (quote (face tabs trailing)))
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((((class color) (background dark)) (:background "grey50" :foreground "darkgray"))))
 '(whitespace-trailing ((((class color) (background dark)) (:background "grey10" :foreground "darkgray")))))

;;; Evil key bindings
;;
;; Since normal state inherits motion state's key bindings,
;; If I want to override a default binding in normal state 
;; with one in motion state, I have to expressly set the old
;; key binding to nil.

(defun my-esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ((or (evil-insert-state-p) (evil-motion-state-p)) [escape])
   ;; This is the best way I could infer for now to have C-c work during evil-read-key.
   ((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
   (t (kbd "C-g"))))
(define-key key-translation-map (kbd "C-c") 'my-esc)

(defun my-insert-bullet ()
  (interactive)
  (ucs-insert "2022"))
(define-key evil-insert-state-map (quote [f4]) 'my-insert-bullet)
(define-key evil-motion-state-map "," 'execute-extended-command)
;; Undo c Evil keybinding for use as prefix key to various Ctrl- key sequences.
(define-key evil-normal-state-map "c" nil)
;; Will use Emacs C-y for paste rather than Evil's evil-scroll-line-up.
(define-key evil-insert-state-map (kbd "C-y") nil)

;; Disable C-0 and C-- since I hit them alot unintentionally.
(define-key evil-motion-state-map (kbd "C-0") 'my-no-op)
(define-key evil-normal-state-map (kbd "C-0") 'my-no-op)
(define-key evil-motion-state-map (kbd "C--") 'my-no-op)
(define-key evil-normal-state-map (kbd "C--") 'my-no-op)

;; I don't use RET in motion state, but it is useful in eg buffer mode.
(define-key evil-motion-state-map (kbd "RET") nil)
(global-set-key (kbd "RET") 'evil-ret)
(define-key evil-normal-state-map "o" nil)
(define-key evil-motion-state-map "o" 'next-buffer)
(define-key evil-normal-state-map "O" nil)
(define-key evil-motion-state-map "O" 'previous-buffer)
(define-key evil-motion-state-map "f" 'buffer-menu)
(define-key evil-normal-state-map "-" nil)
(define-key evil-motion-state-map "-" 'evil-end-of-line)
(define-key evil-normal-state-map "s" nil)
;; Swap p and P, primarily because of how evil-paste-after behaves on empty lines.
(define-key evil-normal-state-map "p" 'evil-paste-before)
(define-key evil-normal-state-map "P" 'evil-paste-after)
(define-key evil-motion-state-map "sg" 'jde-open-class-source)
(define-key evil-motion-state-map "sh" 'highlight-phrase)
(define-key evil-motion-state-map "sex" 'eval-last-sexp)
(define-key evil-motion-state-map "sej" 'paredit-wrap-round)
(define-key evil-motion-state-map "sek" 'paredit-splice-sexp)
(define-key evil-motion-state-map "seh" (lambda () (interactive) (transpose-sexps -1)))
(define-key evil-motion-state-map "sel" (lambda () (interactive) (transpose-sexps 1)))
(define-key evil-motion-state-map "sem" 'mark-sexp)
;; Note: Instead of key binding to kill-sexp, equivalent to 'sem' and then 'd'
(define-key evil-motion-state-map "srb" 'revert-buffer)
(define-key evil-motion-state-map "sle" (lambda () (interactive) (load-file "~/.emacs") (toggle-fullscreen)))
(define-key evil-motion-state-map "sji" 'jde-import-find-and-import)
(define-key evil-motion-state-map "sja" (lambda () (interactive) (jde-import-all) (jde-import-kill-extra-imports) (jde-import-organize)))
;; Use U for redo.  This is meant to mimic a similar line in evil-maps.el .
(when (fboundp 'undo-tree-undo)
   (define-key evil-normal-state-map "U" 'undo-tree-redo))

(define-key evil-normal-state-map ";" nil)
;; Go down in larger steps
(define-key evil-motion-state-map ";" 
   (lambda ()
      (interactive)
      (dotimes (num 8)
         (scroll-up 1)
         (evil-next-line))
      ))
(define-key evil-normal-state-map "'" nil)
;; Go up in larger steps
(define-key evil-motion-state-map "'" 
   (lambda ()
      (interactive)
      (dotimes (num 8) 
         (scroll-down 1)
         (evil-previous-line))
      ))

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
 '((isearch-lazy-highlight-face' :background "yellow" :foreground "black")))

;;; Debug logging
(defun my-insert-ant-log ()
  "Insert log statement for Ant build files. "
  (interactive)
  (insert "<echo message=\"DEBUG: \"/> <!-- TODO: temporary for debug -->")
  (search-backward "DEBUG: ")
  (goto-char (match-end 0)))
(defun my-insert-elisp-log ()
   "Insert log statement for elisp. "
   (interactive)
   (insert "(log-msg (format \"DEBUG: \")) ; TODO: temporary for debug")
   (search-backward "DEBUG: ")
   (goto-char (match-end 0)))
(defun my-insert-java-log ()
   "Insert log statement for Java. "
   (interactive)
   ;; The vimscript was:
   ;;imap <F3> org.slf4j.LoggerFactory.getLogger(this.getClass()).warn( // temporary for debug<Enter><Tab><Tab><Tab>"DEBUG: ",<Enter>new Object[]{} );<Esc>khi
   (insert "org.slf4j.LoggerFactory.getLogger(this.getClass()).warn( // TODO: temporary for debug\n\t\t\t\"DEBUG: \",\n\t\t\tnew Object[]{} );")
   (search-backward "DEBUG: ")
   (goto-char (match-end 0)))
;; For the GOESR program, redefine logger.
;; (fset 'my-insert-java-log 'goesr-insert-java-log)
(defun my-insert-python-log ()
  "Insert log statement for Python. "
  (interactive)
  (insert "print( 'DEBUG: '%() ) # TODO: temporary for debug")
  (search-backward "DEBUG: ")
  (goto-char (match-end 0)))
(defun my-insert-doc-comment ()
  "Insert doc comment /** */ . "
  (interactive)
  (insert "/***/")
  (search-backward "/**")
  (goto-char (match-end 0)))

(add-hook 'emacs-lisp-mode-hook 
   (lambda ()
      (log-msg "Inside emacs-lisp-mode-hook")
      (define-key evil-insert-state-local-map (quote [f3]) 'my-insert-elisp-log)
      (define-key evil-motion-state-local-map (quote [left]) 'backward-sexp)
      (define-key evil-motion-state-local-map (quote [right]) 'forward-sexp)
      (define-key evil-motion-state-local-map (quote [up]) 'backward-up-list)
      (define-key evil-motion-state-local-map (quote [down]) 'down-list)))
(add-hook 'java-mode-hook 
   (lambda ()
      (log-msg "Inside java-mode-hook")
      (require 'jde)
      (define-key evil-insert-state-local-map (quote [f3]) 'my-insert-java-log)))
(add-hook 'nxml-mode-hook
          (lambda ()
            (log-msg "Inside nxml-mode-hook")
            (define-key evil-insert-state-local-map (quote [f3]) 'my-insert-ant-log)))
(add-hook 'python-mode-hook 
   (lambda ()
      (log-msg "Inside python-mode-hook")
      (define-key evil-insert-state-local-map (quote [f3]) 'my-insert-python-log)))
(add-hook 'c-mode-common-hook 
   (lambda ()
      (log-msg "Inside c-mode-common-hook. ")
      (define-key evil-insert-state-local-map (quote [f4]) 'my-insert-doc-comment)))
(add-hook 'after-change-major-mode-hook
   (lambda ()
      ;; Force Evil mode in Fundamental mode.
      (evil-mode 1)))

;;; Finalizing initialization
(add-hook 'term-setup-hook
   (lambda ()
      ;; I tend to put things here that for some reason don't work
      ;; when executed earlier.
     
      (define-key evil-motion-state-map "ch" help-map)
      (define-key evil-motion-state-map "cx" ctl-x-map)
      (delete-other-windows)
      ;;(setq search-whitespace-regexp nil)
      (log-msg "Finished with term-setup-hook. ")))

(log-msg "Finished .emacs file. ")

