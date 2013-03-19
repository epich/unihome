;; -*- lexical-binding: t -*-
;; Lexical binding is necessary for make-conditional-key-translation
;; to create a clojure object correctly.

;;; General emacs settings
(setq visible-bell t) 
(tool-bar-mode 0)
(column-number-mode 1)
;; TODO: auto-fill-mode doesn't work right for debug statement insert commands
;; (auto-fill-mode 1)
;; Disable the auto-save, the #* debris files slow down Emacs startup.
(setq auto-save-default nil)
;; Don't create debris files next to originals.
(setq backup-directory-alist '((".*" . "~/emacs-backup")))
(global-auto-revert-mode 1)
(setq revert-without-query (quote (".*")))
(setq case-replace nil)
(setq vc-follow-symlinks t)
(delete-selection-mode 1)
(setq mouse-yank-at-point t)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq enable-recursive-minibuffers t)
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

;; Load my stuff
(add-to-list 'load-path "~/.emacs.d/my")
(require 'my-util)

(my-toggle-fullscreen)

;; Set the frame title to the current filename.
(setq-default frame-title-format
              '(:eval (format "%s"
                              (file-name-nondirectory (or (buffer-file-name) default-directory)))))

;;; File associations
;;
(add-to-list 'auto-mode-alist '("README.*" . text-mode))
;; Ruby rake build files
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
;; Conjure uses these extensions for Scheme code, for unknown reason
(add-to-list 'auto-mode-alist '("\\.sls\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sps\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("wscript" . python-mode))
(add-to-list 'auto-mode-alist '("Makefile.*" . makefile-mode))

;; (add-to-list 'load-path "~/.emacs.d")
;; Compile .el files if they need to be.
;;
;; From: http://stackoverflow.com/questions/1217180/how-do-i-byte-compile-everything-in-my-emacs-d-directory
;; TODO: When files don't compile, it'll create errors and modest delay everytime Emacs starts.
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(my-msg "Initializing Evil.")
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/evil/lib")
(require 'undo-tree)
(require 'goto-chg)
(require 'evil)
(evil-mode 1)

(my-msg "Initializing Rainbow Delimiters.")
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(require 'rainbow-delimiters)
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;;; Initialize CEDET
;;;
(my-msg "Initializing CEDET.")
(defvar my-enable-cedet-function
  'my-enable-cedet-from-emacs
  "Function to use for loading CEDET.  Determines which CEDET to load. ")

;; CEDET 1.1 is needed when using JDEE
(defun my-enable-cedet-1.1 ()
  "Loads CEDET 1.1. "
  (defvar my-cedet-path "~/.emacs.d/cedet-1.1" "Path to CEDET")
  (add-to-list 'load-path (format "%s/common" my-cedet-path))
  ;; CEDET raises fatal error when reloading an already reloaded file,
  ;; undermining reloading of my init.el file.  This hacks that fix.
  (ignore-errors (load-file (format "%s/common/cedet.el" my-cedet-path)))
  
  ;; Disable Minibuffer info which overwrites other information displaying.
  ;;
  ;; Note, this was a customization setting in custom-set-variables
  ;; for CEDET 1.1, but it is not compatible with newer CEDET.
  ;; '(global-semantic-idle-summary-mode nil nil (semantic-idle))

  ;;; Enable EDE (Project Management) features
  (global-ede-mode 1)
  ;; Enable EDE for a pre-existing C++ project
  ;; (ede-cpp-root-project "NAME" :file "~/proj/name/Makefile")
  ;;; Enabling Semantic (code-parsing, smart completion) features
  ;;; Select one of the following:
  ;; * This enables the database and idle reparse engines
  ;;(semantic-load-enable-minimum-features)
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
  (require 'eassist)
  )

(defun my-enable-cedet-from-emacs ()
  "Loads CEDET distrubted with Emacs."
  ;; Note: Apparently eassist is not distributed with Emacs 24.3
  (require 'semantic/ia)
  (require 'semantic/bovine/gcc)
  (semantic-mode 1)
  (global-ede-mode 1)
  )

;; Experimenting with latest CEDET from their bzr repo
(defun my-enable-cedet-from-bzr ()
  "Loads the latest snapshot of CEDET bzr trunk. "
  (defvar my-cedet-path "/psd15/linux/boreilly/sw/cedet-bzr/trunk" "Path to CEDET")
  (load-file (format "%s/cedet-devel-load.el" my-cedet-path))
  (add-to-list 'load-path (concat my-cedet-path "/contrib"))
  (require 'semantic/ia)
  (require 'semantic/bovine/gcc)
  (semantic-mode 1)
  (require 'eassist)
  (global-ede-mode 1)
  )
;; For now, while trying out CEDET from bzr
(when (eql system-type 'gnu/linux) (setq my-enable-cedet-function 'my-enable-cedet-from-bzr))

;; CEDET documents loading must occur before other packages load any part of CEDET.
;; Especially important since Emacs has a different version builtin, which I can't
;; use when using JDEE.
(ignore-errors (funcall my-enable-cedet-function))

;;; Initialize JDEE
(my-msg "Initializing JDEE.")
(defvar my-jdee-path "~/.emacs.d/jdee-2.4.0.1" "Path to JDEE")
(add-to-list 'load-path (format "%s/lisp" my-jdee-path))
;; Online posting says these might be necessary for JDEE.
;; http://forums.fedoraforum.org/showthread.php?t=280711
;; (defun screen-width nil -1)
(setq jde-check-version-flag nil)
(define-obsolete-function-alias 'make-local-hook 'ignore "21.1")
(unless (fboundp 'semantic-format-prototype-tag-java-mode)
  (defalias 'semantic-format-prototype-tag-java-mode 'semantic-format-tag-prototype-java-mode))
;; To prevent an error with hippie-exp variable not being defined.
(require 'hippie-exp)
(autoload 'jde-mode "jde" "JDE mode." t)
(setq auto-mode-alist
      (append '(("\\.java\\'" . jde-mode)) auto-mode-alist))

;; Initialize project-specific elisp
(my-msg "Initializing project-specific elisp.")
;; GOESR isn't relevant to all computers I work on, so ignore errors.
(ignore-errors (load-file "~/g/goesr-dev.el"))
;; Paths for JDEE
(defvar my-java-classpath (if (boundp 'goesr-classpath)
                              goesr-classpath
                            nil)
  "Classpaths for Java. ")
(defvar my-java-sourcepath (if (boundp 'goesr-sourcepath)
                               goesr-sourcepath
                             nil)
  "Sourcepaths for Java. ")

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

(defun my-insert-bullet ()
  "Insert a Unicode bullet character."
  (interactive)
  ;; Note: Emacs 24.2 and earlier, use: (ucs-insert "2022")
  (insert-char #x2022))

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
;; font-lock-maximum-decoration
;;    C++ was behaving way to slow when simply typing comments.  I lowered
;;    its font-lock-maximum-decoration to 2 and saw massive improvement.
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
 '(Buffer-menu-name-width 64)
 '(ac-delay 1.0)
 '(c-syntactic-indentation nil)
 '(delete-by-moving-to-trash t)
 '(dired-auto-revert-buffer t)
 '(ediff-merge-split-window-function (quote split-window-vertically))
 '(evil-ex-hl-update-delay 0.01)
 '(evil-highlight-closing-paren-at-point-states nil)
 '(evil-intercept-maps nil)
 '(evil-kbd-macro-suppress-motion-error t)
 '(evil-mouse-word (quote evil-move-WORD))
 '(evil-move-cursor-back nil)
 '(evil-overriding-maps nil)
 '(evil-search-module (quote evil-search))
 '(evil-shift-width my-offset)
 '(font-lock-maximum-decoration (quote ((c++-mode . 2))))
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
 '(nxml-sexp-element-flag t)
 '(python-continuation-offset (my-continuation-offset))
 '(python-indent my-offset)
 '(python-indent-offset my-offset)
 '(scroll-conservatively 1)
 '(tags-case-fold-search nil)
 '(undo-tree-visualizer-diff nil)
 '(undo-tree-visualizer-timestamps t)
 '(whitespace-style (quote (face tabs trailing)))
 '(x-select-enable-clipboard t)
 '(x-select-enable-primary t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lazy-highlight ((t (:background "gold" :foreground "black"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "MediumOrchid2"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "MediumOrchid2"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "MediumOrchid2"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "orange"))))
 '(whitespace-tab ((((class color) (background dark)) (:background "grey50" :foreground "darkgray"))))
 '(whitespace-trailing ((((class color) (background dark)) (:background "grey10" :foreground "darkgray")))))

;;; Configure default Evil states for chosen major modes.
;;
;; Change modes that come up in Emacs state to come up in motion state instead.
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)
;; Use Dired in motion state instead of the keymap created in evil-integration.el .
(setq evil-motion-state-modes (cons 'dired-mode evil-motion-state-modes))

;;; Evil key bindings
;;
;; Since normal state inherits motion state's key bindings,
;; If I want to override a default binding in normal state 
;; with one in motion state, I have to expressly set the old
;; key binding to nil.

;;; Set up C- key bindings
;;;
;;; Use Key Translation to create more ergonomic alternatives
;;; to the C- key bindings.  eg "cx" instead of "C-x".
;;;
;;; I originally tried binding directly to the global prefix keymaps (in
;;; term-setup-hook):
;;;   (define-key evil-motion-state-map "cx" ctl-x-map)
;;; but then "cc" doesn't translate to the C-c prefix key of minor modes such
;;; as CEDET Senator's.
;;;
;; First unset Evil's "c" key binding.
(define-key evil-normal-state-map "c" nil)
;; We need to define "c" as a prefix in the evil-motion-state-map
;; in order for our Key Translations to kick in.  Otherwise the
;; Key Lookup completes on simply "c", whether undefined or bound
;; to self-insert-command.
;;
;; When not in motion state, "c" behaves normally because evil-motion-state-map
;; is inactive.
;;
;; If a different keymap defines "c" as a Key Sequence, I can do \c to access it,
;; if the evil-motion-state-map overrides it.
;;
;; If a different keymap defines "c" as a prefix key, the Key Lookup will only
;; use the prefix key's keymap if the next key is not translated.  For example,
;; if I define:
;;    (define-key key-translation-map (kbd "ce") (kbd "C-e"))
;;    (define-key other-mode-map (kbd "cd") (lambda () (interactive) (my-msg "cd command")))
;;    (define-key other-mode-map (kbd "ce") (lambda () (interactive) (my-msg "ce command")))
;; I can use the cd command but not the ce command via the key binding.  I cannot use
;; Evil's \ command because the other-mode-map would still be active in Emacs state.
;; I've never ran into this issue, since prefix keys are usually C- or M- keys.
;;
;; This defines the "c" prefix key in motion state.  The choice of the second key in the
;; sequence and its command binding is somewhat arbitrary.
(define-key evil-motion-state-map "cu" 'universal-argument)
;;; C-c as general purpose escape key sequence.
;;;
(defun my-esc (prompt)
  "Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
  (cond
   ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
   ;; Key Lookup will use it.
   ((or (evil-insert-state-p) (evil-normal-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
   ;; This is the best way I could infer for now to have C-c work during evil-read-key.
   ;; Note: As long as I return [escape] in normal-state, I don't need this.
   ;;((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
   (t (kbd "C-g"))))
(define-key key-translation-map (kbd "C-c") 'my-esc)
;; Works around the fact that Evil uses read-event directly when in operator state, which
;; doesn't use the key-translation-map.
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)
;; Not sure what behavior this changes, but might as well set it, seeing the Elisp manual's
;; documentation of it.
(set-quit-char "C-c")

;;; Other key translations
;;;
(define-key key-translation-map (kbd "omx") (kbd "M-x"))
;; evil-repeat-pop-next isn't particularly useful to me.
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key key-translation-map (kbd "om.") (kbd "M-."))
(define-key key-translation-map (kbd "om;") (kbd "M-;"))

;; Note: lexical-binding must be t in order for this to work correctly.
(defun make-conditional-key-translation (key-from key-to translate-keys-p)
  "Make a Key Translation such that if the translate-keys-p function returns true,
key-from translates to key-to, else key-from translates to itself.  translate-keys-p
takes key-from as an argument. "
  (define-key key-translation-map key-from
    (lambda (prompt)
      (if (funcall translate-keys-p key-from) key-to key-from))))
(defun my-translate-keys-p (key-from)
  "Returns whether conditional key translations should be active.  See make-conditional-key-translation function. "
  (and
   ;; Evil's 'r' command doesn't change evil-state, but we don't want key translations to take effect, otherwise
   ;; replacing a char with g (translated to Ctrl-x) would replace the char with ^X instead.  This check is
   ;; a somewhat hackish way of inferring Evil is in the middle of an evil-read-key call.
   (not (eq overriding-terminal-local-map evil-read-key-map))
   (or (evil-motion-state-p) (evil-normal-state-p) (evil-visual-state-p))))
(defun my-translate-keys-initial-p (key-from)
  "Returns whether conditional key translations should be active; nil if not the initial key of a Key Sequence.  See make-conditional-key-translation function. "
  (and
    ;; Only allow a non identity translation if we're beginning a Key Sequence.
    (equal key-from (this-command-keys))
    (my-translate-keys-p key-from)))
(make-conditional-key-translation (kbd "cc") (kbd "C-c") 'my-translate-keys-p)
;; Create Key Translations for Control keys. Some examples:
;;   (kbd "ch") to (kbd "C-h")
;;   (kbd "cx") to (kbd "C-x")
(if (fboundp 'cl-loop)
    ;; Iterate from ASCII '!' to ASCII '~', minding exceptions:
    ;;  "cm" because it's used for C-M- keys.
    ;;  "cc" because it needs to be in effect throughout a Key Sequence.
    (let ((ascii-exceptions
           (mapcar (lambda (char-arg)
                     (string-to-number (format "%d" char-arg)))
                   '(?c ?m))))
      (cl-loop for ascii-code-i from 33 to 126 by 1 do
               (unless (member ascii-code-i ascii-exceptions)
                 (make-conditional-key-translation (kbd (format "c%c" ascii-code-i))
                                                   (kbd (format "C-%c" ascii-code-i))
                                                   'my-translate-keys-p))
               ;; ascii-exception don't apply to the C-M- case
               (make-conditional-key-translation (kbd (format "cm%c" ascii-code-i))
                                                 (kbd (format "C-M-%c" ascii-code-i))
                                                 'my-translate-keys-p))))

(define-key evil-insert-state-map (kbd "<f4>") 'my-insert-bullet)
;; Will use Emacs C-y for paste rather than Evil's evil-scroll-line-up.
(define-key evil-insert-state-map (kbd "C-y") nil)
;; Disable C-0 and C-- since I hit them alot unintentionally.
(define-key evil-motion-state-map (kbd "C-0") (lambda ()))
(define-key evil-normal-state-map (kbd "C-0") (lambda ()))
(define-key evil-motion-state-map (kbd "C--") (lambda ()))
(define-key evil-normal-state-map (kbd "C--") (lambda ()))

;; Move keybindings between keymaps.
;;
;; In some cases I want key sequences looked up using keymaps other than
;; Evil's, such as RET and SPC in modes that don't involve editing.
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location.

To account for more than one invocation, this won't do the move if key is
nil in keymap-from."
  (let ((keyval (lookup-key keymap-from key)))
    (when keyval
      (define-key keymap-to key keyval)
      (define-key keymap-from key nil))))
;; Want RET to use other keymaps' binding sometimes.  Buffer Menu's for example.
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

(global-set-key (kbd "C-<down>") 'shrink-window)
(global-set-key (kbd "C-<up>") 'enlarge-window)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)

;; ^ in Dired is more useful than Evil's binding.
(define-key evil-motion-state-map "^" nil)
(define-key evil-motion-state-map "f" 'buffer-menu)
(define-key evil-motion-state-map "F" nil)
(define-key evil-motion-state-map "F" 'other-window)
(define-key evil-normal-state-map "-" nil)
(define-key evil-motion-state-map "-" 'evil-end-of-line)
(define-key evil-normal-state-map "s" nil)
(define-key evil-motion-state-map "t" nil)
(define-key evil-motion-state-map "T" nil)
(define-key evil-motion-state-map "t" 'semantic-ia-fast-jump)
;; Swap p and P, primarily because of how evil-paste-after behaves on empty lines.
(define-key evil-normal-state-map "p" 'evil-paste-before)
(define-key evil-normal-state-map "P" 'evil-paste-after)
;; Emacs' undo is more useful in visual state than evil-downcase is.
(define-key evil-visual-state-map "u" nil)
(define-key evil-motion-state-map "," nil)
(define-key evil-motion-state-map "," 'kmacro-end-and-call-macro)
;; Need scroll-conservatively to have a different value when using some Evil commands.
(defadvice evil-window-top (around my-advice-evil-window-top activate)
  (let ((scroll-conservatively 101))
    ad-do-it))
(defadvice evil-window-bottom (around my-advice-evil-window-bottom activate)
  (let ((scroll-conservatively 101))
    ad-do-it))

;;; Merge Evil's g prefix key with Emacs' C-x prefix key.
;; Define key translation to C-x, then add the Evil g bindings to keep.
(make-conditional-key-translation (kbd "g") (kbd "C-x") 'my-translate-keys-initial-p)
(define-key evil-normal-state-map "\C-x&" 'evil-ex-repeat-global-substitute)
(define-key evil-normal-state-map "\C-xa" 'what-cursor-position)
(define-key evil-normal-state-map "\C-xJ" 'evil-join-whitespace)
(define-key evil-normal-state-map "\C-xw" 'evil-fill)
(define-key evil-normal-state-map "\C-xu" 'evil-downcase)
(define-key evil-normal-state-map "\C-xU" 'evil-upcase)
(define-key evil-motion-state-map "\C-xf" 'find-file-at-point)
(define-key evil-motion-state-map "\C-xF" 'evil-find-file-at-point-with-line)
(define-key evil-normal-state-map "\C-x?" 'evil-rot13)
(define-key evil-normal-state-map "\C-x~" 'evil-invert-case)
(define-key evil-normal-state-map "\C-x;" 'goto-last-change)
(define-key evil-normal-state-map "\C-x," 'goto-last-change-reverse)
(define-key evil-motion-state-map "\C-xd" 'evil-goto-definition)
(define-key evil-motion-state-map "\C-xe" 'evil-backward-word-end)
(define-key evil-motion-state-map "\C-xE" 'evil-backward-WORD-end)
(define-key evil-motion-state-map "\C-xg" 'evil-goto-first-line)
(define-key evil-motion-state-map "\C-xj" 'evil-next-visual-line)
(define-key evil-motion-state-map "\C-xk" 'evil-previous-visual-line)
(define-key evil-motion-state-map "\C-x_" 'evil-last-non-blank)
(define-key evil-motion-state-map "\C-x\C-]" 'find-tag)
(define-key evil-motion-state-map "\C-xv" 'evil-visual-restore)

;;; Merge Evil's z prefix key with Emacs' C-c prefix key.
;; Define key translation to C-c, then add the Evil z bindings to keep.
(make-conditional-key-translation (kbd "z") (kbd "C-c") 'my-translate-keys-initial-p)
;; (define-key evil-normal-state-map "\C-co" 'evil-open-fold)
;; (define-key evil-normal-state-map "\C-cc" 'evil-close-fold)
;; (define-key evil-normal-state-map "\C-ca" 'evil-toggle-fold)
;; (define-key evil-normal-state-map "\C-cr" 'evil-open-folds)
;; (define-key evil-normal-state-map "\C-cm" 'evil-close-folds)
(define-key evil-motion-state-map "\C-c^" 'evil-scroll-top-line-to-bottom)
(define-key evil-motion-state-map "\C-c+" 'evil-scroll-bottom-line-to-top)
(define-key evil-motion-state-map "\C-ct" 'evil-scroll-line-to-top)
(define-key evil-motion-state-map "\C-cz" 'evil-scroll-line-to-center)
(define-key evil-motion-state-map "\C-c." "\C-cz^")
(define-key evil-motion-state-map "\C-cb" 'evil-scroll-line-to-bottom)
(define-key evil-motion-state-map "\C-c-" "\C-cb^")

(define-key evil-normal-state-map "o" nil)
(define-key evil-visual-state-map "o" nil)
(define-key evil-normal-state-map "O" nil)
(define-key evil-normal-state-map "oC"
  (lambda ()
    (interactive)
    (call-process "cleartool"
                  nil nil nil
                  "co" "-nc"
                  (format "%s" (file-name-nondirectory (or (buffer-file-name) default-directory))))
    (revert-buffer)))
(define-key evil-motion-state-map "o/" 'highlight-phrase)
(define-key evil-normal-state-map "oa" 'move-past-close-and-reindent)
(define-key evil-normal-state-map "o\"" (lambda (arg) (interactive "P")
                                          (insert-pair arg ?\")))
(define-key evil-normal-state-map "od" 'delete-pair)
(define-key evil-normal-state-map "oj" 'insert-parentheses)
(define-key evil-normal-state-map "ok" 'raise-sexp)
(define-key evil-normal-state-map "oh" (lambda () (interactive) (transpose-sexps -1)))
(define-key evil-normal-state-map "ol" (lambda () (interactive) (transpose-sexps 1)))
(define-key evil-motion-state-map "or" 'revert-buffer)
(define-key evil-motion-state-map "os" 'eassist-switch-h-cpp)
(define-key evil-motion-state-map "oi" (lambda () (interactive) (load-file "~/.emacs.d/init.el") (my-toggle-fullscreen)))
(define-key evil-motion-state-map "ov" 'undo-tree-visualize)
(define-key evil-normal-state-map "S" nil)
(define-key evil-normal-state-map "S" 'save-buffer)

;;; More Evil key bindings

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

;;; Load TAGS file, searching upwards from the directory Emacs was launched.
(let ((my-tags-file (my-find-file-upwards "TAGS")))
  (when my-tags-file
    (my-msg "Loading tags file: %s" my-tags-file)
    (visit-tags-table my-tags-file)))

(require 'my-config)
(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'diff-mode-hook 'my-diff-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'java-mode-hook 'my-java-mode-hook)
(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)
(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
(add-hook 'text-mode-hook 'my-text-mode-hook)
(add-hook 'after-change-major-mode-hook 'my-after-change-major-mode-hook)
(add-hook 'term-setup-hook 'my-term-setup-hook)

(my-msg "Finished loading init file. ")

