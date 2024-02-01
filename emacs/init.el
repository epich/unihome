;; -*- lexical-binding: t -*-

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
;; package-load-list
;;    Merely loading ESS exhibits errors that cause other improper initalizations.
;; scroll-conservatively
;;    Setting is best compromise I've found given the design of automatic
;;    scrolling in the redisplay processing.
;; semantic-idle-scheduler-idle-time
;;    Potentially helps helps disruptive pauses while editing
;; tramp-mode
;;    When lsp-mode and tramp-mode are enabled, Tramp tries to access bogus hosts,
;;    such as a C++ namespace.
;; x-select-enable-clipboard
;;    This is necessary to paste into Windows running on qemu-kvm .
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 64)
 '(ac-delay 1.0)
 '(bazel-buildifier-command "nil")
 '(c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "k&r")))
 '(cc-search-directories
   '("." "/usr/include" "/usr/local/include/*" "./src" "../src" "./include" "../include" "./inc" "../inc" "./public" "../public" "./internal" "../internal"))
 '(clang-format-style
   "{BasedOnStyle: Google, BreakBeforeBinaryOperators: NonAssignment}")
 '(column-number-mode t)
 '(delete-by-moving-to-trash t)
 '(dired-auto-revert-buffer t)
 '(ediff-merge-split-window-function 'split-window-vertically)
 '(evil-ex-hl-update-delay 0.01)
 '(evil-ex-visual-char-range t)
 '(evil-highlight-closing-paren-at-point-states nil)
 '(evil-intercept-maps nil)
 '(evil-kbd-macro-suppress-motion-error t)
 '(evil-mouse-word 'evil-move-WORD)
 '(evil-move-beyond-eol t)
 '(evil-move-cursor-back nil)
 '(evil-overriding-maps nil)
 '(evil-search-module 'evil-search)
 '(evil-shift-width my-offset)
 '(ff-always-try-to-create nil)
 '(ff-special-constructs nil)
 '(ffap-require-prefix t)
 '(fill-column 80)
 '(font-lock-maximum-decoration '((c++-mode . 2)))
 '(frame-background-mode 'dark)
 '(global-whitespace-mode t)
 '(inhibit-startup-screen t)
 '(inverse-video t)
 '(large-file-warning-threshold 1000000000.0)
 '(lsp-document-sync-method ''none)
 '(lsp-enable-codeaction nil)
 '(lsp-enable-flycheck nil)
 '(lsp-ui-peek-enable nil)
 '(message-log-max 100000)
 '(native-comp-async-report-warnings-errors 'silent)
 '(nxml-child-indent my-offset)
 '(nxml-sexp-element-flag t)
 '(package-load-list '(all (ess nil)))
 '(package-selected-packages '(undo-tree lsp-mode diff-hl flylisp adjust-parens evil))
 '(python-indent-offset my-offset)
 '(scroll-conservatively 101)
 '(scroll-margin 4)
 '(select-enable-clipboard t)
 '(select-enable-primary t)
 '(semantic-idle-scheduler-idle-time 60)
 '(semanticdb-default-save-directory (format "%s/semanticdb" my-emacs-data-dir))
 '(sentence-end-double-space nil)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(tags-case-fold-search nil)
 '(tool-bar-mode nil)
 '(undo-tree-visualizer-diff nil)
 '(undo-tree-visualizer-timestamps t)
 '(whitespace-style '(face tabs trailing)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-note ((t nil)))
 '(lazy-highlight ((t (:background "gold" :foreground "black"))))
 '(semantic-idle-symbol-highlight ((t (:background "gray20"))))
 '(whitespace-tab ((((class color) (background dark)) (:background "grey15" :foreground "darkgray"))))
 '(whitespace-trailing ((((class color) (background dark)) (:background "grey10" :foreground "darkgray")))))

;; package.el expects this commented out. my-config.el calls it instead.
;;(package-initialize)

(push "~/unihome/emacs/lisp" load-path)
(require 'my-util)
(require 'my-config)
