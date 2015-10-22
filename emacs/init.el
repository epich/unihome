;; -*- lexical-binding: t -*-
(push "~/unihome/emacs/lisp" load-path)
(require 'my-util)
(require 'my-config)

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
;;    I set jit-lock-defer-time and was able to up font-lock-maximum-decoration
;;    to t again.
;; inverse-video
;;    An attempt to get white on black.  For some reason this doesn't work
;;    but the --reverse-video CLI arg does.
;; scroll-conservatively
;;    Setting is best compromise I've found given the design of automatic
;;    scrolling in the redisplay processing.
;; semantic-idle-scheduler-idle-time
;;    Potentially helps helps disruptive pauses while editing
;; x-select-enable-clipboard
;;    This is necessary to paste into Windows running on qemu-kvm .
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 64)
 '(ac-delay 1.0)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "k&r"))))
 '(cc-search-directories
   (quote
    ("." "/usr/include" "/usr/local/include/*" "./src" "../src" "./include" "../include" "./inc" "../inc" "./public" "../public" "./internal" "../internal")))
 '(column-number-mode t)
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
 '(ffap-require-prefix t)
 '(font-lock-maximum-decoration (quote ((c++-mode . 2))))
 '(frame-background-mode (quote dark))
 '(global-whitespace-mode t)
 '(inhibit-startup-screen t)
 '(inverse-video t)
 '(large-file-warning-threshold 1000000000.0)
 '(message-log-max 100000)
 '(nxml-child-indent my-offset)
 '(nxml-sexp-element-flag t)
 '(python-indent my-offset)
 '(python-indent-offset my-offset)
 '(scroll-conservatively 101)
 '(scroll-margin 4)
 '(semantic-idle-scheduler-idle-time 60)
 '(semanticdb-default-save-directory (format "%s/semanticdb"
                                             my-emacs-data-dir))
 '(sentence-end-double-space nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(tags-case-fold-search nil)
 '(tool-bar-mode nil)
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
 '(semantic-idle-symbol-highlight ((t (:background "gray20"))))
 '(whitespace-tab ((((class color) (background dark)) (:background "grey15" :foreground "darkgray"))))
 '(whitespace-trailing ((((class color) (background dark)) (:background "grey10" :foreground "darkgray")))))
