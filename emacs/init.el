;; -*- lexical-binding: t -*-
;;

(require 'cl-lib)

;;; General emacs settings
(setq visible-bell t) 
(tool-bar-mode 0)
(column-number-mode 1)
(setq print-circle t)
;; TODO: auto-fill-mode doesn't work right for debug statement insert commands
;; (auto-fill-mode 1)
;; Disable the auto-save, the #* debris files slow down Emacs startup.
(setq auto-save-default nil)
(defvar my-emacs-data-dir "~/.emacs.d" "Location of runtime data for Emacs. ")
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
(setq c-default-style "k&r")
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
(add-to-list 'load-path "~/unihome/emacs/my")
(require 'my-util)

(my-toggle-fullscreen)

;; Set the frame title to the current filename.
(setq-default frame-title-format '(:eval (my-get-buffer-name)))

;;; File associations
;;
(add-to-list 'auto-mode-alist '("README.*" . text-mode))
;; Ruby rake build files
(add-to-list 'auto-mode-alist '("\\.clj" . lisp-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rkt" . scheme-mode))
(add-to-list 'auto-mode-alist '("wscript" . python-mode))

(my-msg "Initializing third party lisp. ")
(add-to-list 'load-path "~/unihome/emacs/lisp")
(require 'adjust-parens)
(require 'color-parens)
(require 'evil-numbers)
(require 'goto-chg)
(require 'rainbow-delimiters)
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;;; Undo Tree
(defvar my-use-undo-tree t)
(when my-use-undo-tree
  (my-msg "Initializing Undo Tree. ")
  (require 'undo-tree))

;;; Evil
(my-msg "Initializing Evil.")
(add-to-list 'load-path "~/unihome/emacs/evil")
(require 'evil)
(evil-mode 1)

;;; Initialize CEDET
;;;
(defvar cedet-loaded nil
  "Whether the initialization loaded CEDET explicitly. ")
;; CEDET documents loading must occur before other packages load any part of CEDET.
;; Especially important since Emacs has a different version builtin, which I can't
;; use when using JDEE.
;;
;; CEDET raises an error if loaded again.
(unless cedet-loaded
  (my-msg "Initializing CEDET.")
  ;; When using CEDET source distributed separately from Emacs
  ;;(load-file (format "%s/cedet-devel-load.el" my-bzr-cedet-path))
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
                                    ;;global-semantic-highlight-func-mode
                                    ;; Disabled because don't find it useful.  Looks weird in .mk files.
                                    ;;global-semantic-stickyfunc-mode
                                    ;; There are alternatives for navigating to previous edits
                                    ;;global-semantic-mru-bookmark-mode
                                    ;; Doesn't appear to offer anything currently
                                    ;;global-cedet-m3-minor-mode
                                    ;; Disabled because:
                                    ;;  : Need to customize better face for semantic-idle-symbol-highlight-face
                                    global-semantic-idle-local-symbol-highlight-mode

                                    ;;; For debugging Semantic
                                    ;; global-semantic-show-unmatched-syntax-mode
                                    ;; global-semantic-show-parser-state-mode
                                    ;; global-semantic-highlight-edits-mode
                                    ))
  (semantic-mode 1)
  (global-ede-mode 1)
  (setq cedet-loaded t)
  (setq semanticdb-default-save-directory (format "%s/semanticdb" my-emacs-data-dir))

  ;; Configure GNU Global
  ;; (if (not (cedet-gnu-global-version-check t))
  ;;     (my-msg "WARNING: Failed cedet-gnu-global-version-check ")
  ;;   (semanticdb-enable-gnu-global-databases 'c-mode)
  ;;   (semanticdb-enable-gnu-global-databases 'c++-mode))
  )

;;; Initialize JDEE
(defvar my-jdee-path
  "/psd15/linux/boreilly/sw/jdee-trunk/jdee"
  "Path to JDEE checked out from trunk and built.")
(defvar my-use-jdee
  (file-accessible-directory-p my-jdee-path)
  "Whether to use JDEE. ")
(when my-use-jdee
  (my-msg "Initializing JDEE.")
  (add-to-list 'load-path (format "%s/dist/jdee-2.4.1/lisp" my-jdee-path))
  (autoload 'jde-mode "jde" "JDE mode." t)
  (setq auto-mode-alist
        (append '(("\\.java\\'" . jde-mode)) auto-mode-alist))
  )

;; Initialize project-specific elisp
(my-msg "Initializing project-specific elisp.")
(require 'my-proj)

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

;;; Packaging
;;
;; Use M-x list-packages to manage installed packages
(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)
;; (push '("local-elpa" . "/psd15/linux/boreilly/sw/elpa/packages")
;;       package-archives)

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
;;
;;    I set jit-lock-defer-time and as able to up font-lock-maximum-decoration
;;    to t again.
;; inverse-video
;;    An attempt to get white on black.  For some reason this doesn't work
;;    but the --reverse-video CLI arg does.
;; jit-lock-defer-time
;;    Set to just longer than the keyboard repetition rate.
;; scroll-conservatively
;;    Setting is best compromise I've found given the design of automatic
;;    scrolling in the redisplay processing.
;; x-select-enable-clipboard
;;    This is necessary to paste into Windows running on qemu-kvm .
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 64)
 '(ac-delay 1.0)
 '(cc-search-directories
   (quote
    ("." "/usr/include" "/usr/local/include/*" "./src" "../src" "../../src" "./include" "../include" "./inc" "../inc" "../inc/L1aObject")))
 '(delete-by-moving-to-trash t)
 '(dired-auto-revert-buffer t)
 '(ediff-merge-split-window-function (quote split-window-vertically))
 '(evil-ex-hl-update-delay 0.01)
 '(evil-ex-visual-char-range t)
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
 '(jde-jdk-registry
   (quote
    (("1.6.0" . "/usr/lib/jvm/java-1.6.0-openjdk.x86_64"))))
 '(jde-sourcepath my-java-sourcepath)
 '(jit-lock-defer-time 0.01)
 '(large-file-warning-threshold 1000000000.0)
 '(message-log-max 100000)
 '(nxml-child-indent my-offset)
 '(nxml-sexp-element-flag t)
 '(python-indent my-offset)
 '(python-indent-offset my-offset)
 '(scroll-conservatively 101)
 '(scroll-margin 4)
 '(sentence-end-double-space nil)
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
 '(semantic-idle-symbol-highlight ((t (:background "gray20"))))
 '(whitespace-tab ((((class color) (background dark)) (:background "grey30" :foreground "darkgray"))))
 '(whitespace-trailing ((((class color) (background dark)) (:background "grey10" :foreground "darkgray")))))

(require 'my-config)
(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(add-hook 'text-mode-hook 'my-text-mode-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)
(add-hook 'diff-mode-hook 'my-diff-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'java-mode-hook 'my-java-mode-hook)
(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)
(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
;; Use emacs-startup-hook or eval-after-load?
(add-hook 'emacs-startup-hook 'my-emacs-startup-hook)

(my-msg "Finished loading init file. ")
