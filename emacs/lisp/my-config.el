;; -*- lexical-binding: t -*-
;;
;; My high level config, which can assume other Elisp is loaded.

(require 'cl-lib)
(require 'my-util)

;; On Mac, opening Emacs.app loses the exported PWD env var, whilst
;; other env vars are inherited as expected. unihome.sh smuggles the
;; pwd via this env var instead.
(let ((app-open-pwd (getenv "APP_OPEN_PWD")))
  (when app-open-pwd
    (setq-default default-directory app-open-pwd)))

;;; General emacs settings
;; Not defined for Darwin
(when (fboundp #'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp #'scroll-bar-mode) (scroll-bar-mode 0))

(set-background-color "black")
(set-foreground-color "white")
(setq visible-bell t)
;; Menu bar doesn't take up extra vertical space on Mac
(unless (eq window-system 'ns)
  (menu-bar-mode 0))
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
(setq blink-matching-paren nil)
(setq enable-recursive-minibuffers t)
(fset 'yes-or-no-p 'y-or-n-p)
;; (prefer-coding-system 'utf-8)
;;(setq truncate-lines nil)
(setq window-min-width 80)
(setq split-height-threshold nil)
(setq split-width-threshold 80)
;;(setq mac-command-modifier 'meta)
;; Disable inappropriate behavior for left click in inactive
;; minibuffer (opens *Messages*)
(define-key minibuffer-inactive-mode-map [mouse-1] nil)
(setq eglot-ignored-server-capabilites '(:hoverProvider :signatureHelpProvider))

;; Font
;;
;; - On small screen (Macbook and Emacs configured with --with-ns)
;; - On large screen (Gnome workstation)
;;
;; TODO: Use a better criteria
(defvar my-font (if (eq window-system 'ns) "Monospace 10" "Monospace 8"))

;; Works on Windows? If not, make it conditional
(set-frame-font my-font nil t)
;; With default RHEL 5 font of Sans, :height 72 seems to be the minimum that 'B' and '8' can be distinguished.
;; The :height is basically 10 times font size
;; (set-face-attribute 'default nil :family "DejaVu LGC Sans Mono" :height 72)
;; '(default ((t (:family "DejaVu LGC Sans Mono" :foundry "unknown" :slant normal :weight normal :height 80 :width normal))))

;;; Version specific elisp
;; electric-pair-mode introduced in version 24.
;;
;; Disabling, see my emacs.txt notes for some of the things to address first.
(cond ((<= 24 emacs-major-version)
       (electric-pair-mode 0)))

(my-toggle-fullscreen)

;; Set the frame title to the current filename.
(setq-default frame-title-format '(:eval (my-get-buffer-name)))

;;; File associations
;;
(push '("README.*" . text-mode) auto-mode-alist)
;; Ruby rake build files
(push '("\\.clj" . lisp-mode) auto-mode-alist)
(push '("Rakefile" . ruby-mode) auto-mode-alist)
(push '("\\.rkt" . scheme-mode) auto-mode-alist)
(push '("wscript" . python-mode) auto-mode-alist)
(push '("\\.log" . text-mode) auto-mode-alist)

;;; Relating to tabs
;; Permanently force Emacs to indent with spaces, never with TABs:
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (cdr (number-sequence 0 256 my-offset)))
(setq-default c-basic-offset my-offset)
;; Doesn't work here, works in custom-set-variables.
;;(setq evil-shift-width my-offset)
;; Determines how to display tabs.
;;
;; It's best to use the default, for Python editing and because some Emacs elisp code is formatted on that assumption.
;;(setq tab-width my-offset)
;; Disable weird auto formatting
(setq-default c-electric-flag nil)
(electric-indent-mode -1)

;;; Packaging
;;
;; Use M-x list-packages to manage installed packages
(require 'package)
(push '("melpa" . "https://melpa.org/packages/")
      package-archives)
;; (push '("melpa-stable" . "http://stable.melpa.org/packages/")
;;       package-archives)
;; (push '("local-elpa" . "/psd15/linux/boreilly/sw/elpa/packages")
;;       package-archives)

;; Any package customizations must precede this.
(package-initialize)
;; Emacs manual says to to set this to nil if manually calling
;; package-initialize
(setq package-enable-at-startup nil)
;; If there are no archives downloaded, then do so.
;;
;; This is for bootstrapping a new Emacs installation. In the steady
;; state, don't contact the remote repos every startup. Downloading
;; updates to the archive contents can be done by:
;;   M-x list-packages
;;
;; If the (my-package-load 'evil) fails, do M-x list-packages and see
;; if evil is downloaded and listed.
(unless package-archive-contents
  (package-refresh-contents))

(my-package-load 'evil)
;; my-package-load required evil, but we do it again to avoid a flood
;; of compiler warnings.
;;
;; TODO: build.py errors on this for first time run. Workaround is to emacs -q,
;; eval-last-sexp:
;;   (require 'package)
;;   (push '("melpa" . "http://melpa.milkbox.net/packages/")
;;         package-archives)
;; list-packages and install evil
(require 'evil)
(when (featurep 'evil)
  (evil-mode 1))
(my-package-load 'adjust-parens)
(my-package-load 'flylisp)

;; Initialize project-specific elisp
(my-msg "Initializing project-specific elisp.")
;; TODO: Improve this: look for telltale files like lisp/subr.el
;; TODO: Maybe look for .git, though don't rely on it because of tarballs
;; (defvar my-project-root
;;         (or (my-find-file-upwards "emacs")
;;             "unihome" "trunk" "sw")
;;         "Path to current project. " )
(push "~/mygoog/lisp" load-path)
(require 'google-project nil t)

;;; Configure default Evil states for chosen major modes.
;;
;; Change modes that come up in Emacs state to come up in motion state instead.
(setq evil-motion-state-modes
      (append '(;; Use Dired in motion state instead of the keymap created in evil-integration.el.
                dired-mode
                eassist-mode
                xref--xref-buffer-mode)
              evil-emacs-state-modes
              evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

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
;; Want RET to use other keymaps' binding sometimes. Buffer Menu's for example.
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

(define-key evil-motion-state-map "Y" 'kill-ring-save)

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
(define-key evil-motion-state-map "os" 'ff-find-other-file)
(define-key evil-motion-state-map "oi" (lambda () (interactive) (load-file "~/.emacs") (my-toggle-fullscreen)))
(define-key evil-normal-state-map "S" nil)
(define-key evil-motion-state-map " " nil)
(define-key evil-normal-state-map " " 'save-buffer)

(defvar my-clipboard-val "")
(defun my-copy (start end)
  (interactive (list (region-beginning)
                     (region-end)))
  (setq my-clipboard-val (filter-buffer-substring start end))
  (setq deactivate-mark t)
  (cond ((called-interactively-p 'interactive)
         (indicate-copied-region))))
(defun my-paste (&optional arg)
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (not current-prefix-arg))))
  (push-mark)
  (insert my-clipboard-val)
  (if (not arg) (exchange-point-and-mark)))
;; "y" command defaults to evil-normal-state-map, which doesn't work in non
;; editing buffers
(define-key evil-normal-state-map "y" nil)
(define-key evil-motion-state-map "y" 'my-copy)
(define-key evil-normal-state-map "p" 'my-paste)

;;; More Evil key bindings

(define-key evil-motion-state-map (kbd "C-y") nil)

;; overwrite-mode (insert key) can supercede Vim's R command.
(define-key evil-normal-state-map "R" nil)
(define-key evil-motion-state-map "R" 'revert-buffer)

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
;; Too easily inputted accidentally, yet bound to evil-save-modified-and-close
(define-key evil-normal-state-map "ZZ" nil)

(defvar my-leap-scroll-size 16)
(define-key evil-normal-state-map "J" nil)
(define-key evil-motion-state-map "J" 
  (lambda ()
    (interactive)
    (scroll-up my-leap-scroll-size)
    (evil-next-line my-leap-scroll-size)))
(define-key evil-normal-state-map "K" nil)
(define-key evil-motion-state-map "K" 
  (lambda ()
    (interactive)
    (scroll-down my-leap-scroll-size)
    (evil-previous-line my-leap-scroll-size)))
(define-key evil-motion-state-map (kbd "<down>")
  (lambda ()
    (interactive)
    (scroll-up-line)
    (evil-next-line)))
(define-key evil-motion-state-map (kbd "<up>")
  (lambda ()
    (interactive)
    (scroll-down-line)
    (evil-previous-line)))


;; Dired mapping to open all subdirs recursively
(require 'dired)
(defun my-insert-subdir-r ()
  (interactive)
  (let ((dired-listing-switches (concat dired-listing-switches "R")))
    (dired dired-directory dired-listing-switches)))
(define-key dired-mode-map "r" 'my-insert-subdir-r)

;;; Load TAGS file, searching upwards from the directory Emacs was launched.
(let ((my-tags-file (my-find-file-upwards
                     "TAGS"
                     ;; Workaround a file system that falsly reports
                     ;; files as existing as a dir
                     (lambda (possible-file)
                       (not (file-directory-p possible-file))))))
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
(defun my-insert-makefile-log ()
  "Insert log statement for make files. "
  (interactive)
  (insert "$(warning DEBUG: ) # TODO: temporary for debug")
  (search-backward "DEBUG: ")
  (goto-char (match-end 0)))
(defun my-insert-python-log ()
  "Insert log statement for Python. "
  (interactive)
  (insert "print( \"DEBUG: \".format() ) ; sys.stdout.flush() # TODO: temporary for debug")
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

(defvar my-cedet-loaded nil "Whether my Elisp loaded CEDET.")
;; Semantic is too buggy to enable if we have alternative code
;; navigation: Grok or Kythe using lsp-mode.
(defun my-cedet-init ()
  (unless (or my-cedet-loaded (featurep 'grok) (featurep 'lsp-mode)
              (featurep 'google-lsp))
    (my-msg "Loading CEDET packages.")
    ;; When using CEDET source distributed separately from Emacs
    ;;(load-file (format "%s/cedet-devel-load.el" my-bzr-cedet-path))
    
    (require 'semantic)
    (require 'semantic/idle)
    (require 'semantic/db-mode)

    (define-key evil-motion-state-map "t" nil)
    (define-key evil-motion-state-map "T" nil)
    (define-key evil-motion-state-map "t" 'semantic-ia-fast-jump)
    (define-key evil-motion-state-map "T" 'semantic-ia-show-summary)
    
    ;; Note: Instead of setting any semantic-default-submodes prior to
    ;; starting semantic-mode, the "submodes" (really minor modes) are
    ;; started in major mode hooks. This is because some of the Semantic
    ;; minor modes are not useful or even annoying in other major modes.
    (setq semantic-default-submodes nil)
    (semantic-mode 1)
    ;;(global-ede-mode 1)
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
    (setq my-cedet-loaded t)))

;; TODO: Does this help?
(defun my-eglot-flymake-backend (report-fn &rest _more)
  "Override because syntax highlighting doesn't behave well."
  (cond (eglot--managed-mode
         (setq eglot--current-flymake-report-fn report-fn)
         ;; Report anything unreported
         (when eglot--unreported-diagnostics
           (eglot--report-to-flymake (cdr eglot--unreported-diagnostics))))
        (t
         (funcall report-fn nil))))

(defun my-c-mode-common-hook ()
  (my-msg "Inside my-c-mode-common-hook for buffer %s " (buffer-name))
  ;; TODO: Project specific:
  ;;(define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-c-log)
  (define-key evil-insert-state-local-map (kbd "<f4>") 'my-insert-cc-doc)
  (my-bind-tab-del-keys)
  ;; Set to just longer than the keyboard repetition rate.
  (setq jit-lock-defer-time 0.01)
  ;; TODO: Reassess whether I ever want CEDET
  ;; (my-cedet-init)
  (modify-syntax-entry ?_ "w")
  ;; Override this function because syntax highlighting doesn't behave well.
  ;; (defun eglot-flymake-backend (report-fn &rest _more))
  )
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
  (when (featurep 'adjust-parens)
    (adjust-parens-mode 1))
  (when (featurep 'flylisp)
    (flylisp-mode 1)))
(defun my-go-mode-hook ()
  (my-msg "Inside my-go-mode-hook for buffer %s " (buffer-name))
  ;; Indentation with tabs is typical, improve readability with smaller tab-width
  (setq tab-width 2)
  (setq window-min-width 120))
(defun my-java-mode-hook ()
  (my-msg "Inside my-java-mode-hook for buffer %s " (buffer-name))
  (define-key evil-insert-state-local-map (kbd "<f3>") 'my-insert-java-log))

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

  (when (my-package-load 'diff-hl)
    (global-diff-hl-mode 1))
  (when (my-package-load 'undo-tree)
    (global-undo-tree-mode -1))
  (my-package-load 'goto-chg)

  ;;(setq search-whitespace-regexp nil)

  ;; Make the end obvious, since this is a major point in the Emacs runtime
  (my-msg "---------------- Finished with my-emacs-startup-hook. ----------------")
  )

(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(add-hook 'text-mode-hook 'my-text-mode-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'clojure-mode-hook 'my-clojure-mode-hook)
(add-hook 'diff-mode-hook 'my-diff-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'java-mode-hook 'my-java-mode-hook)
(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)
(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
;; Use emacs-startup-hook or eval-after-load?
(add-hook 'emacs-startup-hook 'my-emacs-startup-hook)

(my-msg "Finished loading init file. ")
(provide 'my-config)
