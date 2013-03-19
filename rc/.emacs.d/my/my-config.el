;; My high level config, which can assume other Elisp is loaded.

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
   (insert "(my-msg \"DEBUG: \") ")
   (search-backward "DEBUG: ")
   (goto-char (match-end 0)))
(defun my-insert-c-log ()
   "Insert log statement for C and C++. "
   (interactive)
   ;; This is the simplest way I could find to get a proper and complete current time.
   (insert "{ timespec debug_ts; char debug_dateStr[20]; { ::clock_gettime(CLOCK_REALTIME, &debug_ts); tm mytm; ::localtime_r(&debug_ts.tv_sec, &mytm); ::strftime(debug_dateStr, 20, \"%Y-%m-%dT%H:%M:%S\", &mytm); }")
   (evil-ret)
   (insert "  printf( \"%s.%09ld|pid:%d|tid:%ld|%s|%d| DEBUG: \\n\", // TODO: debugging")
   (evil-ret)
   (insert "          debug_dateStr, debug_ts.tv_nsec, ::getpid(), ::pthread_self(), __FILE__, __LINE__ ); fflush(stdout); }")
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
   (insert "new Object[]{} );")
   (search-backward "DEBUG: ")
   (goto-char (match-end 0)))
;; For the GOESR program, redefine logger.
;; (fset 'my-insert-java-log 'goesr-insert-java-log)
(defun my-insert-makefile-log ()
  "Insert log statement for make files. "
  (interactive)
  (insert "$(warning DEBUG: ) # TODO: temporary for debug")
  (search-backward "DEBUG: ")
  (goto-char (match-end 0)))
(defun my-insert-python-log ()
  "Insert log statement for Python. "
  (interactive)
  (insert "print( \"DEBUG: \"%() ) ; sys.stdout.flush() # TODO: temporary for debug")
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

(defun my-prog-mode-hook ()
  (my-msg "Inside my-prog-mode-hook")
  ;; We want special tab behavior in programming modes, because the usefulness
  ;; just barely out weights the annoyances.
  (define-key evil-insert-state-local-map (kbd "DEL") nil)
  (define-key evil-insert-state-local-map (kbd "TAB") nil)
  )

(defun my-c-mode-common-hook ()
  (my-msg "Inside my-c-mode-common-hook. ")
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-c-log)
  (define-key evil-insert-state-local-map (kbd "<f4>") 'my-insert-cc-doc)
  (my-bind-tab-del-keys)
  )

(defun my-diff-mode-hook ()
  (define-key evil-motion-state-local-map "sj" 'diff-file-next)
  (define-key evil-motion-state-local-map "sk" 'diff-file-prev)
  (define-key evil-motion-state-local-map "sb" 'diff-refine-hunk)
  (define-key evil-motion-state-local-map "sc" 'diff-goto-source)
  (define-key evil-motion-state-local-map "se" 'diff-ediff-patch)
  (define-key evil-motion-state-local-map "sr" 'diff-reverse-direction)
  (define-key evil-motion-state-local-map "sw" 'diff-ignore-whitespace-hunk)
  ;; Define and [ and ] to be similar to sj and sk, but move beginning of file diff to top visible line.
  (define-key evil-motion-state-map "[" nil)
  (define-key evil-motion-state-map "[" "skzt")
  (define-key evil-motion-state-map "]" nil)
  (define-key evil-motion-state-map "]" "sjzt")
  )
(defun my-emacs-lisp-mode-hook ()
  (my-msg "Inside my-emacs-lisp-mode-hook")
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-elisp-log)
  (define-key evil-motion-state-local-map "se" 'eval-last-sexp)
  )
(defun my-java-mode-hook ()
  (my-msg "Inside my-java-mode-hook")
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-java-log)

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
  (define-key evil-normal-state-local-map "sa" (lambda () (interactive) (jde-import-all) (jde-import-kill-extra-imports) (jde-import-organize)))
  )
(defun my-makefile-mode-hook ()
  (my-msg "Inside my-makefile-mode-hook")
  ;; Tabs are important in makefiles
  ;;
  ;; Set tab to nil, to get the builtin tab behavior.
  (define-key evil-insert-state-local-map (kbd "TAB") nil)
  (setq indent-tabs-mode t)

  ;; Get rid of annoying messages when saving makefiles
  ;;
  ;; Not sufficient to fset directly in upper level of init.el .
  ;; Fsetting these misguided buggers here seems to work.
  (fset 'makefile-warn-suspicious-lines (lambda ()))
  (fset 'makefile-warn-continuations (lambda ()))

  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-makefile-log)
  )
(defun my-nxml-mode-hook ()
  (my-msg "Inside my-nxml-mode-hook")
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-ant-log)
  )
(defun my-python-mode-hook ()
  (my-msg "Inside my-python-mode-hook")
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-python-log)
  (define-key evil-insert-state-local-map (kbd "<f4>") 'my-insert-python-doc)
  )
(defun my-sh-mode-hook ()
  (my-msg "Inside my-sh-mode-hook")
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-sh-log)
  )
(defun my-text-mode-hook ()
  (my-msg "Inside my-text-mode-hook")
  (my-bind-tab-del-keys)
  )

(defun my-after-change-major-mode-hook ()
  ;; Force Evil mode in Fundamental mode.
  (evil-mode 1)
  )

;;; Finalizing initialization
(defun my-term-setup-hook ()
  ;; Apparently some elisp needs to be placed here to work.

  (delete-other-windows)

  ;;(setq search-whitespace-regexp nil)
  (my-msg "Finished with my-term-setup-hook. ")
  )

(provide 'my-config)

