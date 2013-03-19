;; -*- lexical-binding: t -*-
;;
;; Utility functions for my Emacs init file.

(require 'cl)

;;; Functions to facilitate elisp debug logging.
(defvar my-date-time-format "%Y-%m-%dT%H:%M:%S"
  "Format for date string. ")
(defun my-get-usec-str (cur-time)
   "Get the microseconds as string. "
   (format "%06d"
      (nth 2 cur-time)))
(defun my-get-time-str ()
   "Get the current time as a string. "
   (interactive)
   (let ((cur-time (current-time)))
      (format "%s.%s" 
         (format-time-string my-date-time-format)
         (my-get-usec-str cur-time))))
;; I attempted to use defadvice on the message function, but the minibuffer
;; misbehaves under some conditions.  The message function is a C primitive
;; anyway, which doesn't always combine with defadvice.
(defun my-msg (msg &rest vargs)
   "Log a message, with prepended information.  Used for debugging. "
   (interactive)
   (message "%s %s" (my-get-time-str) (apply 'format msg vargs)))

;; Maximize window upon startup.  A non toggling way to do this would be nice.
(defun my-x-toggle-fullscreen ()
  "Toggle fullscreen in X11. "
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
(defun my-msw-toggle-fullscreen ()
  "MS Windows fullscreen function. "
  (w32-send-sys-command 61488))
(defun my-toggle-fullscreen ()
  (interactive)
  ;; When cl-case is available, use that for a bit more cleanliness.
  (cond
   ((eql system-type 'aix)           (my-x-toggle-fullscreen))
   ((eql system-type 'berkeley-unix) (my-x-toggle-fullscreen))
   ((eql system-type 'cygwin)        (my-msw-toggle-fullscreen))
   ((eql system-type 'darwin)        (my-x-toggle-fullscreen))
   ((eql system-type 'gnu)           (my-x-toggle-fullscreen))
   ((eql system-type 'gnu/linux)     (my-x-toggle-fullscreen))
   ((eql system-type 'gnu/kfreebsd)  (my-x-toggle-fullscreen))
   ((eql system-type 'hpux)          (my-x-toggle-fullscreen))
   ((eql system-type 'irix)          (my-x-toggle-fullscreen))
   ((eql system-type 'ms-dos)        (my-msw-toggle-fullscreen))
   ((eql system-type 'usg-unix-v)    (my-x-toggle-fullscreen))
   ((eql system-type 'windows-nt)    (my-msw-toggle-fullscreen))))

(defun my-find-file-upwards (file-to-find)
  "Recursively searches each parent directory starting from the default-directory.
looking for a file with name file-to-find.  Returns the path to it
or nil if not found."
  (cl-labels
      ((find-file-r (path)
                    (let* ((parent (file-name-directory path))
                           (possible-file (concat parent file-to-find)))
                      (cond
                       ((file-exists-p possible-file) possible-file) ; Found
                       ;; The parent of ~ is nil and the parent of / is itself.
                       ;; Thus the terminating condition for not finding the file
                       ;; accounts for both.
                       ((or (null parent) (equal parent (directory-file-name parent))) nil) ; Not found
                       (t (find-file-r (directory-file-name parent))))))) ; Continue
    (find-file-r default-directory)))

;; TODO: Make the *match-list* variables and functions conform to my- prefix standard.

(defvar match-list nil
  "A list of matches, as set through the set-match-list and consumed by the cycle-match-list function. ")
(defvar match-list-iter nil
  "Iterator through the global match-list variable. ")
(defun reset-match-list-iter ()
  "Set match-list-iter to the beginning of match-list and return it. "
  (interactive)
  (setq match-list-iter match-list))
(defun make-match-list (match-regexp use-regexp beg end)
  "Set the match-list variable as described in the documentation for set-match-list. "
  ;; Starts at the beginning of region, searches forward and builds match-list.
  ;; For efficiency, matches are appended to the front of match-list and then reversed
  ;; at the end.
  ;;
  ;; Note that the behavior of re-search-backward is such that the same match-list
  ;; is not created by starting at the end of the region and searching backward.
  (let ((match-list nil))
    (save-excursion
      (goto-char beg)
      (while
          (let ((old-pos (point)) (new-pos (re-search-forward match-regexp end t)))
            (when (equal old-pos new-pos)
              (error "re-search-forward makes no progress.  old-pos=%s new-pos=%s end=%s match-regexp=%s"
                     old-pos new-pos end match-regexp))
            new-pos)
        (setq match-list
              (cons (replace-regexp-in-string match-regexp
                                              use-regexp
                                              (match-string 0)
                                              t)
                    match-list)))
      (setq match-list (nreverse match-list)))))
(defun set-match-list (match-regexp use-regexp beg end)
  "Set the match-list global variable to a list of regexp matches.  MATCH-REGEXP
is used to find matches in the region from BEG to END, and USE-REGEXP is the
regexp to place in the match-list variable.

For example, if the region contains the text: {alpha,beta,gamma}
and MATCH-REGEXP is: \\([a-z]+\\),
and USE-REGEXP is: \\1
then match-list will become the list of strings: (\"alpha\" \"beta\")"
  (interactive "sMatch regexp: \nsPlace in match-list: \nr")
  (setq match-list (make-match-list match-regexp use-regexp beg end))
  (reset-match-list-iter))
(defun cycle-match-list (&optional after-end-string)
  "Return the next element of match-list.

If AFTER-END-STRING is nil, cycle back to the beginning of match-list.
Else return AFTER-END-STRING once the end of match-list is reached."
  (let ((ret-elm (car match-list-iter)))
    (unless ret-elm
      (if after-end-string
          (setq ret-elm after-end-string)
        (reset-match-list-iter)
        (setq ret-elm (car match-list-iter))))
    (setq match-list-iter (cdr match-list-iter))
    ret-elm))
(defadvice replace-regexp (before my-advice-replace-regexp activate)
  "Advise replace-regexp to support match-list functionality. "
  (reset-match-list-iter))

(defun my-bind-tab-del-keys ()
  "Bind the TAB and DEL keys because default behaviors are shitty. "
     ;; (define-key evil-insert-state-map (kbd "DEL") 'backward-delete-char-untabify)
     (define-key evil-insert-state-local-map (kbd "DEL") 'backspace-whitespace-to-tab-stop)
     ;; Tab behavior is too retarded in several major modes.  Either it is unncessarily
     ;; restrictive about allowing tabbing, or it aligns with the line above in the wrong cases.
     (define-key evil-insert-state-local-map (kbd "TAB") 'tab-to-tab-stop))

(defun surround-region-with-tag (tag-name beg end)
  "Insert XML tag named tag-name around region defined by beg end. "
  (interactive "sTag name: \nr")
  (save-excursion
    (goto-char beg)
    (insert "<" tag-name ">")
    (goto-char (+ end 2 (length tag-name)))
    (insert "</" tag-name ">")))

(provide 'my-util)

