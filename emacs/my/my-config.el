;; -*- lexical-binding: t -*-
;;
;; My high level config, which can assume other Elisp is loaded.

(require 'cl-lib)
(require 'my-util)
(require 'evil)

;;; Configure default Evil states for chosen major modes.
;;
;; Change modes that come up in Emacs state to come up in motion state instead.
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)
;; Use Dired in motion state instead of the keymap created in evil-integration.el .
(setq evil-motion-state-modes (cons 'dired-mode evil-motion-state-modes))
(setq evil-motion-state-modes (cons 'eassist-mode evil-motion-state-modes))

(defun my-bind-tab-del-keys ()
  "Bind the TAB and DEL keys because default behaviors are shitty. "
     ;; (define-key evil-insert-state-map (kbd "DEL") 'backward-delete-char-untabify)
     (define-key evil-insert-state-local-map (kbd "DEL") 'backspace-whitespace-to-tab-stop)
     ;; Tab behavior is too retarded in several major modes.  Either it is unncessarily
     ;; restrictive about allowing tabbing, or it aligns with the line above in the wrong cases.
     (define-key evil-insert-state-local-map (kbd "TAB") 'tab-to-tab-stop))

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
(defun my-esc (_prompt)
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
;; TODO: Doesn't work in terminals. If it turns out I need it, check (display-graphic-p)
;;(set-quit-char "C-c")

;;; Other key translations
;;;
(define-key key-translation-map (kbd "omx") (kbd "M-x"))
;; evil-repeat-pop-next isn't particularly useful to me.
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key key-translation-map (kbd "om.") (kbd "M-."))
(define-key key-translation-map (kbd "om;") (kbd "M-;"))

(defun my-translate-keys-p (_key-from)
  "Returns whether conditional key translations should be active.  See make-conditional-key-translation function. "
  (and
   ;; Evil's 'r' command doesn't change evil-state, but we don't want key translations to take effect, otherwise
   ;; replacing a char with g (translated to Ctrl-x) would replace the char with ^X instead.  This check is
   ;; a somewhat hackish way of inferring Evil is in the middle of an evil-read-key call.
   (not (eq overriding-local-map evil-read-key-map))
   (not (eq overriding-terminal-local-map isearch-mode-map))
   (or (evil-motion-state-p) (evil-normal-state-p) (evil-visual-state-p))))
(defun my-translate-keys-initial-p (key-from)
  "Returns whether conditional key translations should be active; nil if not the initial key of a Key Sequence.  See make-conditional-key-translation function. "
  (and
    ;; Only allow a non identity translation if we're beginning a Key Sequence.
    (equal key-from (this-command-keys))
    (my-translate-keys-p key-from)))
(make-conditional-key-translation (kbd "cc") (kbd "C-c") 'my-translate-keys-p)
(make-conditional-key-translation "c " (kbd "C-SPC") 'my-translate-keys-p)
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

(global-set-key (kbd "<f4>") 'my-insert-bullet)
;; Will use Emacs C-y for paste rather than Evil's evil-scroll-line-up.
(define-key evil-insert-state-map (kbd "C-y") nil)
;; Disable C-0 and C-- since I hit them alot unintentionally.
(define-key evil-motion-state-map (kbd "C-0") (lambda ()))
(define-key evil-normal-state-map (kbd "C-0") (lambda ()))
(define-key evil-motion-state-map (kbd "C--") (lambda ()))
(define-key evil-normal-state-map (kbd "C--") (lambda ()))

(global-set-key (kbd "RET") 'newline-and-indent)
;; Want RET to use other keymaps' binding sometimes.  Buffer Menu's for example.
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")
;; "y" command defaults to evil-normal-state-map, which prevents it from working in non editing buffers
(my-move-key evil-normal-state-map evil-visual-state-map "y")

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
(define-key evil-motion-state-map "T" 'semantic-ia-show-summary)
;; Swap p and P, primarily because of how evil-paste-after behaves on empty lines.
(define-key evil-normal-state-map "p" 'evil-paste-before)
(define-key evil-normal-state-map "P" 'evil-paste-after)
;; Emacs' undo is more useful in visual state than evil-downcase is.
(define-key evil-visual-state-map "u" nil)
(define-key evil-motion-state-map "," nil)
(define-key evil-motion-state-map "," 'kmacro-end-and-call-macro)

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
                  (buffer-file-name))
    ;; Get the new write permissions
    (revert-buffer)))
(defmacro my-define-insert-pair-key-binding (open-char close-char)
  "Define key binding for inserting a pair."
  `(define-key evil-normal-state-map ,(format "o%c" open-char)
     (lambda (&optional parg)
       (interactive "*P")
       (insert-pair parg ,open-char ,close-char))))
(my-define-insert-pair-key-binding ?\" ?\")
(my-define-insert-pair-key-binding ?\' ?\')
(my-define-insert-pair-key-binding ?\( ?\))
(my-define-insert-pair-key-binding ?\[ ?\])
(my-define-insert-pair-key-binding ?\{ ?\})
(my-define-insert-pair-key-binding ?\< ?\>)
(define-key evil-motion-state-map "o-" 'evil-numbers/dec-at-pt)
(define-key evil-motion-state-map "o=" 'evil-numbers/inc-at-pt)
(define-key evil-motion-state-map "o/" 'highlight-phrase)
(define-key evil-normal-state-map "oa" 'move-past-close-and-reindent)
(define-key evil-normal-state-map "o\"" (lambda (arg) (interactive "P")
                                          (insert-pair arg ?\")))
(define-key evil-normal-state-map "od" 'delete-pair)
(define-key evil-normal-state-map "oj" 'insert-parentheses)
(define-key evil-normal-state-map "ok" 'raise-sexp)
(define-key evil-normal-state-map "oh" (lambda () (interactive) (transpose-sexps -1)))
(define-key evil-normal-state-map "ol" (lambda () (interactive) (transpose-sexps 1)))
(define-key evil-motion-state-map "opf" 'elp-instrument-function)
(define-key evil-motion-state-map "opp" 'elp-instrument-package)
(define-key evil-motion-state-map "opr" 'elp-results)
(define-key evil-motion-state-map "or" 'revert-buffer)
(define-key evil-motion-state-map "os" 'ff-find-other-file)
(define-key evil-motion-state-map "oi" (lambda () (interactive) (load-file "~/.emacs") (my-toggle-fullscreen)))
(define-key evil-normal-state-map "S" nil)
(define-key evil-motion-state-map " " nil)
(define-key evil-normal-state-map " " 'save-buffer)

;;; More Evil key bindings

;;; Undo system key bindings
(define-key evil-normal-state-map "u" nil)
(define-key evil-motion-state-map "u" 'undo-only)
(define-key evil-visual-state-map "U" nil)
;; TODO: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16377
(if nil ;(featurep 'undo-tree)
    (progn
      (define-key evil-motion-state-map "U" 'undo-tree-redo)
      (define-key evil-motion-state-map "ov" 'undo-tree-visualize))
  (define-key evil-motion-state-map "U" 'undo))

(defvar my-leap-scroll-size 16)
(define-key evil-normal-state-map ";" nil)
(define-key evil-motion-state-map ";" 
  (lambda ()
    (interactive)
    (scroll-up my-leap-scroll-size)
    (evil-next-line my-leap-scroll-size)))
(define-key evil-normal-state-map "'" nil)
(define-key evil-motion-state-map "'" 
  (lambda ()
    (interactive)
    (scroll-down my-leap-scroll-size)
    (evil-previous-line my-leap-scroll-size)))

;; Dired mapping to open all subdirs recursively
(require 'dired)
(defun my-insert-subdir-r ()
  (interactive)
  (let ((dired-listing-switches (concat dired-listing-switches "R")))
    (dired dired-directory dired-listing-switches)))
(define-key dired-mode-map "r" 'my-insert-subdir-r)

;;; Load TAGS file, searching upwards from the directory Emacs was launched.
(let ((my-tags-file (my-find-file-upwards "TAGS")))
  (when my-tags-file
    (my-msg "Loading tags file: %s" my-tags-file)
    (visit-tags-table my-tags-file)))

;; TODO: Fixed in CEDET mainline and Emacs trunk, so delete this when in a formal Emacs release
(defadvice semantic-change-function (around my-advice-semantic-change-function activate)
  (save-match-data ad-do-it))

;; Want the behavior of (toggle-c-auto-newline 1) for braces, but not for semicolon
(defadvice c-electric-brace (around my-advice-c-electric-brace activate)
  ;; Dynamic let
  (let ((c-electric-flag t) (c-auto-newline))
    ad-do-it))

;; Want to actually see the directory contents I go to
(defadvice dired-maybe-insert-subdir (after my-advice-dired-maybe-insert-subdir activate)
  (evil-scroll-line-to-top nil))

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
   ;; Insert a debug statement that doesn't require any of my own functions
   ;; TODO: Use my-msg without duplicating its code here
   (insert "(let ((cur-time (current-time))) (message \"%s.%s DEBUG: \" (format-time-string \"%Y-%m-%dT%H:%M:%S\" cur-time) (format \"%06d\" (nth 2 cur-time))))")
   (search-backward "DEBUG: ")
   (goto-char (match-end 0)))
(defun my-insert-c-log ()
   "Insert log statement for C and C++. "
   (interactive)
   ;; This is the simplest way I could find to get a proper and complete current time.
   ;; Requires these includes: <pthread.h> <unistd.h> <stdio.h> <time.h>
   (insert "{ struct timespec debug_ts; char debug_dateStr[20]; { clock_gettime(CLOCK_REALTIME, &debug_ts); struct tm mytm; localtime_r(&debug_ts.tv_sec, &mytm); strftime(debug_dateStr, 20, \"%Y-%m-%dT%H:%M:%S\", &mytm); }")
   (newline-and-indent)
   (insert "  printf( \"%s.%09ld|pid:%d|tid:%ld|%s|%d| DEBUG: \\n\", // TODO: debugging")
   (newline-and-indent)
   (insert "          debug_dateStr, debug_ts.tv_nsec, getpid(), pthread_self(), __FILE__, __LINE__ ); fflush(stdout); }")
   (search-backward "DEBUG: ")
   (goto-char (match-end 0)))
(defun my-insert-java-log ()
   "Insert log statement for Java. "
   (interactive)
   ;; The vimscript was:
   ;;imap <F3> org.slf4j.LoggerFactory.getLogger(this.getClass()).warn( // temporary for debug<Enter><Tab><Tab><Tab>"DEBUG: ",<Enter>new Object[]{} );<Esc>khi
   (insert "org.slf4j.LoggerFactory.getLogger(this.getClass()).warn( // TODO: temporary for debug")
   (newline-and-indent)
   (insert "\t\t\t\"DEBUG: \",")
   (newline-and-indent)
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
(defun my-insert-ruby-log ()
  "Insert log statement for Ruby. "
  (interactive)
  (insert "printf( \"DEBUG: \" ) # TODO: temporary for debug")
  (search-backward "DEBUG: ")
  (goto-char (match-end 0)))
(defun my-insert-sh-log ()
  "Insert log statement for shell. "
  (interactive)
  (insert "# TODO: temporary for debug")
  (newline-and-indent)
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
  (newline-and-indent)
  (newline-and-indent)
  (insert "Keyword arguments:")
  (newline-and-indent)
  (insert "\"\"\"")
  (newline-and-indent)
  (search-backward "\"\"\"")
  (search-backward "\"\"\"")
  (goto-char (match-end 0)))

(defun my-prog-mode-hook ()
  (my-msg "Inside my-prog-mode-hook")
  ;; We want special tab behavior in programming modes, because the usefulness
  ;; just barely out weights the annoyances.
  (define-key evil-insert-state-local-map (kbd "DEL") nil)
  (define-key evil-insert-state-local-map (kbd "TAB") nil)
  ;; Note: Don't modify ?- for all languages because then the
  ;; evil-ex-search-symbol-forward command will search (for example):
  ;; "foo-" of foo->method()
  (modify-syntax-entry ?_ "w")
  (outline-minor-mode 1)
  )
(defun my-text-mode-hook ()
  (my-msg "Inside my-text-mode-hook for buffer %s " (buffer-name))
  (my-bind-tab-del-keys)
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?\" "$")
  )

(defun my-c-mode-common-hook ()
  (my-msg "Inside my-c-mode-common-hook for buffer %s " (buffer-name))
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-c-log)
  (define-key evil-insert-state-local-map (kbd "<f4>") 'my-insert-cc-doc)
  (my-bind-tab-del-keys)
  ;; Set to just longer than the keyboard repetition rate.
  (setq jit-lock-defer-time 0.01)

  ;; Semantic minor modes
  ;;
  ;; So far, I have only found Semantic useful in C, C++, Java
  (when cedet-loaded
    (global-semantic-idle-scheduler-mode 1)
    (global-semanticdb-minor-mode 1)
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
    ;; Disabled because it causes point to move in header files. The fact that
    ;; it highlights namespaces, particularly std, is also annoying.
    ;;(global-semantic-idle-local-symbol-highlight-mode 1)
    ;; For debugging Semantic
    ;; global-semantic-show-unmatched-syntax-mode
    ;; global-semantic-show-parser-state-mode
    ;; global-semantic-highlight-edits-mode
    ))
(defun my-clojure-mode-hook ()
  (my-msg "Inside my-clojure-mode-hook for buffer %s " (buffer-name))
  (define-key evil-motion-state-local-map "se" 'nrepl-eval-last-expression)
  (local-set-key (kbd "TAB") 'lisp-indent-adjust-parens)
  (local-set-key (kbd "<backtab>") 'lisp-dedent-adjust-parens))
(defun my-diff-mode-hook ()
  (my-msg "Inside my-diff-mode-hook for buffer %s " (buffer-name))
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
  (my-msg "Inside my-emacs-lisp-mode-hook for buffer %s " (buffer-name))
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-elisp-log)
  (define-key evil-motion-state-local-map "se" 'eval-last-sexp)
  (modify-syntax-entry ?- "w")
  )
(defun my-java-mode-hook ()
  (my-msg "Inside my-java-mode-hook for buffer %s " (buffer-name))
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
  (define-key evil-normal-state-local-map "sa" (lambda ()
                                                 (interactive)
                                                 (jde-import-all)
                                                 (jde-import-kill-extra-imports)
                                                 (jde-import-organize)))
  (define-key evil-normal-state-local-map "sg" 'jde-open-class-at-point))

(defun my-makefile-mode-hook ()
  (my-msg "Inside my-makefile-mode-hook for buffer %s " (buffer-name))
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
  (modify-syntax-entry ?- "w")
  )
(defun my-nxml-mode-hook ()
  (my-msg "Inside my-nxml-mode-hook for buffer %s " (buffer-name))
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-ant-log)
  (modify-syntax-entry ?- "w")
  )
(defun my-python-mode-hook ()
  (my-msg "Inside my-python-mode-hook for buffer %s " (buffer-name))
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-python-log)
  (define-key evil-insert-state-local-map (kbd "<f4>") 'my-insert-python-doc)
  )
(defun my-ruby-mode-hook ()
  (my-msg "Inside my-ruby-mode-hook for buffer %s " (buffer-name))
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-ruby-log)
  )
(defun my-sh-mode-hook ()
  (my-msg "Inside my-sh-mode-hook for buffer %s " (buffer-name))
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-sh-log)
  )

;;; Finalizing initialization
(defun my-emacs-startup-hook ()
  ;; Apparently some elisp needs to be placed here to work.

  (delete-other-windows)

  ;;(setq search-whitespace-regexp nil)

  ;; Make the end obvious, since this is a major point in the Emacs runtime
  (my-msg "---------------- Finished with my-emacs-startup-hook. ----------------")
  )

(provide 'my-config)

