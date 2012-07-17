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
(fset 'yes-or-no-p 'y-or-n-p)
;;(setq truncate-lines nil)

;;; Version specific elisp
;; electric-pair-mode introduced in version 24.
;;
;; Disabling, see my emacs.txt notes for some of the things to address first.
(cond ((<= 24 emacs-major-version)
       (electric-pair-mode 0)))

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
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;;; Initialize CEDET
;;;
(log-msg "Initializing CEDET.")
(defvar my-cedet-path "~/.emacs.d/cedet-1.1" "Path to CEDET")
(add-to-list 'load-path (format "%s/common" my-cedet-path))
;; CEDET documents loading must occur before other packages load any part of CEDET.
;; Especially important since Emacs has a different version builtin, which I can't
;; use because of JDEE.
;;
;; CEDET raises fatal error when reloading an already reloaded file,
;; undermining reloading of my .emacs file.  This hacks that fix.
(ignore-errors (load-file (format "%s/common/cedet.el" my-cedet-path)))
;;; Enable EDE (Project Management) features
(global-ede-mode 1)
;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/proj/name/Makefile")
;;; Enabling Semantic (code-parsing, smart completion) features
;;; Select one of the following:
;; * This enables the database and idle reparse engines
;; (semantic-load-enable-minimum-features)
;; * This enables some tools useful for coding, such as summary mode,
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)
;; * This enables even more coding tools such as intellisense mode,
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-gaudy-code-helpers)
;;; Based on advice at http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;; For smart completion
;; (require 'semantic-ia)
;; Solves error when semantic-complete-jump:
;;    Symbol's function definition is void: eieio-build-class-alist
(require 'eieio-opt)

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
   ;; a problem yet.  Emacs documents Elib is a part of Emacs.
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

;;; Initialize Clearcase extensions
;;
;; Initializes slowly (byte compiled).  I observed 12 seconds for 17 script files in a snapshot view.
(defun my-clearcase-checkout ()
  (interactive)
  (clearcase-commented-checkout buffer-file-name "Checked out through Emacs. ")
  )
(defun my-load-clearcase ()
   (log-msg "Initializing ClearCase.")
   (add-to-list 'load-path "~/.emacs.d/clearcase")
   ;; clearcase.el uses obsolete variable.  This works around it.
   (defvar directory-sep-char ?/)
   (require 'clearcase)
   (clearcase-mode 1)
   ;; Key bindings.  Little rhyme or reason except to choose unclaimed keys.
   (define-key clearcase-prefix-map "n" 'clearcase-checkin-current-buffer)
   (define-key clearcase-prefix-map "o" 'my-clearcase-checkout)
   (log-msg "Finished initializing ClearCase.")
   )

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
(defun backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (cond
    (indent-tabs-mode
      (call-interactively 'backward-delete-char-untabify))
    ((region-active-p)
      (call-interactively 'backward-delete-char-untabify))
    (t
       (let ((movement (% (current-column) my-offset))
            (p (point)))
        (when (= movement 0) (setq movement my-offset))
        ;; Account for edge case near beginning of buffer
        (setq movement (min (- p 1) movement))
        (save-match-data
          (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
              (backward-delete-char (- (match-end 1) (match-beginning 1)))
            (call-interactively 'backward-delete-char)))))))
;; (define-key evil-insert-state-map (kbd "DEL") 'backward-delete-char-untabify)
(define-key evil-insert-state-map (kbd "DEL") 'backspace-whitespace-to-tab-stop)
;; Tab behavior is too retarded in several major modes.  Either it is unncessarily
;; restrictive about allowing tabbing, or it aligns with the line above in the wrong cases.
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
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
 '(c-syntactic-indentation nil)
 '(evil-overriding-maps nil)
 '(evil-intercept-maps nil)
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

;;; Set up C-c keymappings
(defun my-esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ((or (evil-insert-state-p) (evil-motion-state-p)) [escape])
   ;; This is the best way I could infer for now to have C-c work during evil-read-key.
   ((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
   (t (kbd "C-g"))))
(define-key key-translation-map (kbd "C-c") 'my-esc)
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
(set-quit-char "C-c")

;;; Other key translations
;;;
;;; Use key translation when I want a key sequence to do whatever it is another key sequence does.
;;
;; Originally tried binding directly to the mode-specific-map, which is the global C-c prefix key,
;; but then "cc" doesn't translate to the C-c prefix key of minor modes such as CEDET Senator's.
;; Key translation works instead.
(define-key key-translation-map (kbd "cc") (kbd "C-c"))
(define-key key-translation-map (kbd "ch") (kbd "C-h"))
(define-key key-translation-map (kbd "cx") (kbd "C-x"))
;; C-M-x is major mode dependant, but generally binds to the elisp function that
;; instruments a function for the debugger.
(define-key key-translation-map (kbd "cmx") (kbd "C-M-x"))
;; (define-key key-translation-map (kbd "cs") (kbd "C-s"))
(define-key key-translation-map (kbd "cu") (kbd "C-u"))
(define-key key-translation-map (kbd "smx") (kbd "M-x"))
;; (define-key evil-normal-state-map (kbd "M-.") nil)
(define-key key-translation-map (kbd "sm.") (kbd "M-."))

;; Configure default Evil states for chosen major modes.
;;
;; For now removing all, and can add back in as needed or until
;; I understand the philosophy of the feature better.
(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
;; Remove a major mode
;; (delete 'debugger-mode evil-emacs-state-modes)

(defun my-insert-bullet ()
  (interactive)
  (ucs-insert "2022"))
(define-key evil-insert-state-map (kbd "<f4>") 'my-insert-bullet)
;; Undo c Evil keybinding for use with keybindings beginning with "c".
;; The Emacs Ctrl- prefix keybindings are assigned to Evil c keybindings later.
;; The Emacs Ctrl- keybindings to commands are assigned here.
(define-key evil-normal-state-map "c" nil)
;; Will use Emacs C-y for paste rather than Evil's evil-scroll-line-up.
(define-key evil-insert-state-map (kbd "C-y") nil)
;; Disable C-0 and C-- since I hit them alot unintentionally.
(define-key evil-motion-state-map (kbd "C-0") 'my-no-op)
(define-key evil-normal-state-map (kbd "C-0") 'my-no-op)
(define-key evil-motion-state-map (kbd "C--") 'my-no-op)
(define-key evil-normal-state-map (kbd "C--") 'my-no-op)

;; Move RET and SPC from the motion to normal keymaps.  Do so primarily
;; so as the major modes that come up in motion state don't have these
;; defined and will use their own key bindings.  For example, in Buffer
;; Menu, I want RET to use the Buffer Menu's RET key binding instead of
;; evil-ret.  SPC in Edebug mode is another example.
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil)
  )
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

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
(define-key evil-motion-state-map "," nil)
(define-key evil-motion-state-map "," 'kmacro-end-and-call-macro)
(define-key evil-motion-state-map "sg" 'jde-open-class-source)
(define-key evil-motion-state-map "sh" 'highlight-phrase)
(define-key evil-motion-state-map "sex" 'eval-last-sexp)
(define-key evil-normal-state-map "sej" 'paredit-wrap-round)
(define-key evil-normal-state-map "sek" 'paredit-splice-sexp)
(define-key evil-normal-state-map "seh" (lambda () (interactive) (transpose-sexps -1)))
(define-key evil-normal-state-map "sel" (lambda () (interactive) (transpose-sexps 1)))
(define-key evil-motion-state-map "sem" 'mark-sexp)
;; Note: Instead of key binding to kill-sexp, equivalent to 'sem' and then 'd'
(define-key evil-motion-state-map "srb" 'revert-buffer)
(define-key evil-motion-state-map "sle" (lambda () (interactive) (load-file "~/.emacs") (toggle-fullscreen)))
(define-key evil-motion-state-map "slc" (lambda () (interactive) (my-load-clearcase)))
(define-key evil-normal-state-map "sji" 'jde-import-find-and-import)
(define-key evil-normal-state-map "sja" (lambda () (interactive) (jde-import-all) (jde-import-kill-extra-imports) (jde-import-organize)))
;; Use U for redo.  This is meant to mimic a similar line in evil-maps.el .
(when (fboundp 'undo-tree-undo)
   (define-key evil-normal-state-map "U" 'undo-tree-redo))

(define-key evil-normal-state-map ";" nil)
;; Go down in larger steps
(define-key evil-motion-state-map ";" 
   (lambda ()
      (interactive)
      (let ((num-times 8))
        (scroll-up num-times)
        (evil-next-line num-times))
      ))
(define-key evil-normal-state-map "'" nil)
;; Go up in larger steps
(define-key evil-motion-state-map "'" 
   (lambda ()
      (interactive)
      (let ((num-times 8))
        (scroll-down num-times)
        (evil-previous-line num-times))
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
(defun my-insert-c-log ()
   "Insert log statement for C and C++. "
   (interactive)
   (insert "printf( \"%s:%d:DEBUG: \\n\", // TODO: temporary for debug")
   (evil-ret)
   (insert "\t\t\t__FILE__, __LINE__ );")
   (search-backward "DEBUG: ")
   (goto-char (match-end 0)))
(defun my-insert-java-log ()
   "Insert log statement for Java. "
   (interactive)
   ;; The vimscript was:
   ;;imap <F3> org.slf4j.LoggerFactory.getLogger(this.getClass()).warn( // temporary for debug<Enter><Tab><Tab><Tab>"DEBUG: ",<Enter>new Object[]{} );<Esc>khi
   (insert "org.slf4j.LoggerFactory.getLogger(this.getClass()).warn( // TODO: temporary for debug")
   (evil-ret)
   (insert "\t\t\t\"DEBUG: \",")
   (evil-ret)
   (insert "\t\t\tnew Object[]{} );")
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
(defun my-insert-sh-log ()
  "Insert log statement for shell. "
  (interactive)
  (insert "# TODO: temporary for debug")
  (evil-ret)
  (insert "echo \"DEBUG: \"")
  (search-backward "DEBUG: ")
  (goto-char (match-end 0)))
(defun my-insert-cc-doc ()
  "Insert doc comment for C like languages in the form of /** */"
  (interactive)
  (insert "/***/")
  (search-backward "/**")
  (goto-char (match-end 0)))
(defun my-insert-python-doc ()
  "Insert docstring for Python."
  (interactive)
  (insert "\"\"\"")
  (evil-ret)
  (evil-ret)
  (insert "Keyword arguments:")
  (evil-ret)
  (insert "\"\"\"")
  (evil-ret)
  (search-backward "\"\"\"")
  (search-backward "\"\"\"")
  (goto-char (match-end 0)))

(add-hook 'emacs-lisp-mode-hook 
   (lambda ()
      (log-msg "Inside emacs-lisp-mode-hook")
      (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-elisp-log)
      (define-key evil-motion-state-local-map (kbd "<left>") 'backward-sexp)
      (define-key evil-motion-state-local-map (kbd "<right>") 'forward-sexp)
      (define-key evil-motion-state-local-map (kbd "<up>") 'backward-up-list)
      (define-key evil-motion-state-local-map (kbd "<down>") 'down-list)))
(add-hook 'java-mode-hook 
   (lambda ()
      (log-msg "Inside java-mode-hook")
      (require 'jde)
      (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-java-log)))
(add-hook 'nxml-mode-hook
          (lambda ()
            (log-msg "Inside nxml-mode-hook")
            (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-ant-log)))
(add-hook 'python-mode-hook 
   (lambda ()
      (log-msg "Inside python-mode-hook")
      ;; I'm not sure I like tab behavior in Python, so redefine tab.
      ;;
      ;; Emacs' bzr trunk has significant changes to the Python major mode, so I'll
      ;; reevaluate what to do about tabbing in Python once I've tried it out.
      ;; (define-key evil-insert-state-local-map (kbd "TAB") 'tab-to-tab-stop)
      (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-python-log)
      (define-key evil-insert-state-local-map (kbd "<f4>") 'my-insert-python-doc)))
(add-hook 'c-mode-common-hook 
   (lambda ()
      (log-msg "Inside c-mode-common-hook. ")
      (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-c-log)
      (define-key evil-insert-state-local-map (kbd "<f4>") 'my-insert-cc-doc)
      ))
(add-hook 'sh-mode-hook 
   (lambda ()
      (log-msg "Inside sh-mode-hook")
      (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-sh-log)))
(add-hook 'text-mode-hook 
   (lambda ()
      (log-msg "Inside text-mode-hook")
      ))
(add-hook 'after-change-major-mode-hook
   (lambda ()
      ;; Force Evil mode in Fundamental mode.
      (evil-mode 1)))

;;; Finalizing initialization
(add-hook 'term-setup-hook
   (lambda ()
     ;; I tend to put things here that for some reason don't work
     ;; when executed earlier.

     ;; Using the Emacs builtin keymaps, I find I have to do them in this hook in order to work.
     ;;
     ;; In particular, these don't work when placed above:
     ;;
     ;; (define-key evil-motion-state-map "cc" (lambda () (interactive) mode-specific-map))
     ;;        ; Reason: "cc" does nothing, probably returns variable but doesn't know to use it as a keymap.
     ;; (define-key evil-motion-state-map "cc" mode-specific-map)
     ;;        ; Reason: Looks in function cell of mode-specific-map, but it's a variable.
     ;; (define-key evil-motion-state-map "cc" 'mode-specific-map)
     ;;        ; Reason: Looks in function cell of mode-specific-map, but it's a variable.
     ;;
     ;; I am commenting out this binding approach, because it conflicts with
     ;; other minor modes somewhat often.
     ;; (define-key evil-motion-state-map "cc" mode-specific-map)
     ;; (define-key evil-motion-state-map "ch" help-map)
     ;; (define-key evil-motion-state-map "cx" ctl-x-map)
     (delete-other-windows)
     ;;(setq search-whitespace-regexp nil)
     (log-msg "Finished with term-setup-hook. ")))

(log-msg "Finished .emacs file. ")

