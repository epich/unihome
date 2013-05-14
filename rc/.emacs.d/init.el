;; -*- lexical-binding: t -*-
;;

(require 'cl)

;;; General emacs settings
(setq visible-bell t) 
(tool-bar-mode 0)
(column-number-mode 1)
;; TODO: auto-fill-mode doesn't work right for debug statement insert commands
;; (auto-fill-mode 1)
;; Disable the auto-save, the #* debris files slow down Emacs startup.
(setq auto-save-default nil)
(defvar my-emacs-data-dir "~/emacs-data" "Location of runtime data for Emacs. ")
;; Don't create debris files next to originals.
(setq backup-directory-alist `((".*" . ,(format "%s/backup" my-emacs-data-dir))))
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
(setq-default frame-title-format '(:eval (my-get-buffer-name)))

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

(my-msg "Initializing third party lisp. ")
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'rainbow-delimiters)
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(require 'undo-tree)
(require 'goto-chg)

(my-msg "Initializing Evil.")
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;;; Initialize CEDET
;;;
(defvar my-enable-cedet-function
  'my-enable-cedet-from-emacs
  "Function to use for loading CEDET.  Determines which CEDET to load. ")
(defvar cedet-loaded nil
  "Whether the initialization loaded CEDET explicitly. ")

;; CEDET 1.1 is needed when using JDEE
(defun my-enable-cedet-1.1 ()
  "Loads CEDET 1.1. "
  (defvar my-cedet-path "~/.emacs.d/cedet-1.1" "Path to CEDET")
  (add-to-list 'load-path (format "%s/common" my-cedet-path))
  (load-file (format "%s/common/cedet.el" my-cedet-path))
  
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
  (setq cedet-loaded t)
  )

(defun my-enable-cedet-from-emacs ()
  "Loads CEDET distrubted with Emacs."
  ;; Note: Apparently eassist is not distributed with Emacs 24.3
  (require 'semantic/ia)
  (require 'semantic/bovine/gcc)
  (semantic-mode 1)
  (global-ede-mode 1)
  (setq cedet-loaded t)
  (setq semanticdb-default-save-directory (format "%s/semanticdb" my-emacs-data-dir))
  )

(defvar my-bzr-cedet-path "/goesr/user/boreilly/sw/cedet" "Path to CEDET")
;; Experimenting with latest CEDET from their bzr repo
(defun my-enable-cedet-from-bzr ()
  "Loads the latest snapshot of CEDET bzr trunk. "
  (load-file (format "%s/cedet-devel-load.el" my-bzr-cedet-path))
  (add-to-list 'load-path (concat my-bzr-cedet-path "/contrib"))
  (require 'semantic/ia)
  (require 'semantic/bovine/gcc)
  (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                    global-semanticdb-minor-mode
                                    ;; Disabled because it obstructs the minibuffer
                                    ;;global-semantic-idle-summary-mode
                                    ;; Disabled in favor of manual invocation
                                    ;;global-semantic-idle-completions-mode
                                    ;; Disabled because of annoying tag boundary.  Other decorations seem incomplete
                                    ;;  : semantic-tag-boundary is annoying
                                    ;;  : semantic-decoration-on-(private|protected)-members does not decorate uses
                                    ;;  : semantic-decoration-on-includes highlights system includes in red
                                    ;;global-semantic-decoration-mode
                                    ;; Disabled because don't find it useful.
                                    ;; global-semantic-highlight-func-mode
                                    ;; Disabled because don't find it useful.  Looks weird in .mk files.
                                    ;; global-semantic-stickyfunc-mode
                                    global-semantic-mru-bookmark-mode
                                    global-cedet-m3-minor-mode
                                    ;; Disabled because:
                                    ;;  : Need to customize better face for semantic-idle-symbol-highlight-face
                                    ;; global-semantic-idle-local-symbol-highlight-mode
                                    ))
  (semantic-mode 1)
  (require 'eassist)
  (global-ede-mode 1)
  (setq cedet-loaded t)
  (setq semanticdb-default-save-directory "/goesr/user/boreilly/semanticdb")

  ;; Configure GNU Global
  ;; (if (not (cedet-gnu-global-version-check t))
  ;;     (my-msg "WARNING: Failed cedet-gnu-global-version-check ")
  ;;   (semanticdb-enable-gnu-global-databases 'c-mode)
  ;;   (semanticdb-enable-gnu-global-databases 'c++-mode))
  )

(defvar my-use-jdee
  ;; Returns t iff we are opening a .java file from CLI args
  (eval `(or ,@(mapcar (lambda (arg) (string-match "\\.java" arg)) command-line-args)))
  "Whether to use JDEE. ")
(defvar my-load-goesr (getenv "LOAD_GOESR_ELISP") "Whether initialization loads GOESR Elisp. ")
(cond
 ;; If we're initially opening a .java file, load CEDET 1.1 which JDEE needs
 (my-use-jdee
  (setq my-enable-cedet-function 'my-enable-cedet-1.1))
 ;; Load CEDET from bzr if available
 ((file-accessible-directory-p my-bzr-cedet-path)
  (setq my-enable-cedet-function 'my-enable-cedet-from-bzr))
 ;; Otherwise load builtin CEDET (my-enable-cedet-function already set)
 )

;; CEDET documents loading must occur before other packages load any part of CEDET.
;; Especially important since Emacs has a different version builtin, which I can't
;; use when using JDEE.
;;
;; CEDET raises an error if loaded again.
(unless cedet-loaded
  (my-msg "Initializing CEDET with %s function." my-enable-cedet-function)
  (funcall my-enable-cedet-function))

;;; Initialize JDEE
(when my-use-jdee
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
        (append '(("\\.java\\'" . jde-mode)) auto-mode-alist)))

;; Initialize project-specific elisp
(when my-load-goesr
  (my-msg "Initializing project-specific elisp.")
  (load-file "/goesr/user/boreilly/goesr-dev.el")
  (require 'my-proj)
  )

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
 '(cc-search-directories (quote ("." "/usr/include" "/usr/local/include/*" "./src" "../src" "../../src" "./include" "../include" "./inc" "../inc" "../inc/L1aObject")))
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
 '(ff-always-try-to-create nil)
 '(ff-special-constructs nil)
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
(add-hook 'term-setup-hook 'my-term-setup-hook)

(my-msg "Finished loading init file. ")

