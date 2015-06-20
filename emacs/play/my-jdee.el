;; Retired code for initializing JDEE, including old non builtin
;; version of CEDET it relies on.

;;; Initialize CEDET
;;;
(defvar cedet-loaded nil "Whether my Elisp loaded CEDET.")
;; CEDET documents loading must occur before other packages load any part of CEDET.
;; Especially important since Emacs has a different version builtin, which I can't
;; use when using JDEE.
;;
;; CEDET raises an error if loaded again.
(when (and my-use-cedet (not cedet-loaded))
  (my-msg "Initializing CEDET.")
  ;; When using CEDET source distributed separately from Emacs
  ;;(load-file (format "%s/cedet-devel-load.el" my-bzr-cedet-path))
  (require 'semantic/ia)
  (require 'semantic/bovine/gcc)
  ;; Note: Instead of setting any semantic-default-submodes prior to
  ;; starting semantic-mode, the "submodes" (really minor modes) are
  ;; started in major mode hooks. This is because some of the Semantic
  ;; minor modes are not useful or even annoying in other major modes.
  (setq semantic-default-submodes nil)
  ;; See if this helps disruptive pauses while editing
  (setq semantic-idle-scheduler-idle-time 60)
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
  (and my-use-cedet (file-accessible-directory-p my-jdee-path))
  "Whether to use JDEE. ")
(when my-use-jdee
  (my-msg "Initializing JDEE.")
  (push (format "%s/dist/jdee-2.4.1/lisp" my-jdee-path) load-path)
  (autoload 'jde-mode "jde" "JDE mode." t)
  (setq auto-mode-alist
        (append '(("\\.java\\'" . jde-mode)) auto-mode-alist))
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

(custom-set-variables
 '(jde-global-classpath my-java-classpath)
 '(jde-jdk-registry
   (quote
    (("1.6.0" . "/usr/lib/jvm/java-1.6.0-openjdk.x86_64"))))
 '(jde-sourcepath my-java-sourcepath)
 )

(defun my-java-mode-hook ()
  ;;; Based on jde-key-bindings from jde.el, which are not in any keymap by default:
  (define-key evil-normal-state-local-map "sCa" 'jde-run-menu-run-applet)
  (define-key evil-normal-state-local-map "sCb" 'jde-build)
  (define-key evil-normal-state-local-map "sCc" 'jde-compile)
  (define-key evil-normal-state-local-map "sCd" 'jde-debug)
  (define-key evil-normal-state-local-map "sCf" 'jde-find)
  (define-key evil-normal-state-local-map "sCg" 'jde-open-class-at-point)
  (define-key evil-normal-state-local-map "sCk" 'jde-bsh-run)
  (define-key evil-normal-state-local-map "sCl" 'jde-gen-println)
  (define-key evil-normal-state-local-map "sCn" 'jde-help-browse-jdk-doc)
  (define-key evil-normal-state-local-map "sCp" 'jde-save-project)
  (define-key evil-normal-state-local-map "sCq" 'jde-wiz-update-class-list)
  (define-key evil-normal-state-local-map "sCr" 'jde-run)
  (define-key evil-normal-state-local-map "sCs" 'speedbar-frame-mode)
  (define-key evil-normal-state-local-map "sCt" 'jde-jdb-menu-debug-applet)
  (define-key evil-normal-state-local-map "sCw" 'jde-help-symbol)
  (define-key evil-normal-state-local-map "sCx" 'jde-show-superclass-source)
  (define-key evil-normal-state-local-map "sCy" 'jde-open-class-at-point)
  (define-key evil-normal-state-local-map "sCz" 'jde-import-find-and-import)
  (define-key evil-normal-state-local-map "se"    'jde-wiz-extend-abstract-class)
  (define-key evil-normal-state-local-map "sf"    'jde-gen-try-finally-wrapper)
  (define-key evil-normal-state-local-map "si"    'jde-wiz-implement-interface)
  (define-key evil-normal-state-local-map "sj"    'jde-javadoc-autodoc-at-line)
  (define-key evil-normal-state-local-map "so"    'jde-wiz-override-method)
  (define-key evil-normal-state-local-map "st"    'jde-gen-try-catch-wrapper)
  (define-key evil-normal-state-local-map "sz"    'jde-import-all)
  (define-key evil-normal-state-local-map "sc[" 'jde-run-etrace-prev)
  (define-key evil-normal-state-local-map "sc]" 'jde-run-etrace-next)
  (define-key evil-normal-state-local-map "sc." 'jde-complete)
  (define-key evil-normal-state-local-map "s." 'jde-complete-in-line)
  ;; My own
  (define-key evil-normal-state-local-map "sa" (lambda ()
                                                 (interactive)
                                                 (jde-import-all)
                                                 (jde-import-kill-extra-imports)
                                                 (jde-import-organize)))
  (define-key evil-normal-state-local-map "sg" 'jde-open-class-at-point))
