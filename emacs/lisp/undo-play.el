;; To benchmark undo in region:
;;   - open *scratch*
;;   - call play-insert
;;   - elp-instrument-function undo-make-selective-list
;;   - select third line
;;   - undo in region twice
;;   - elp-results
;;
;; Three trials with old undo-make-selective-list code:
;;   0.22304 0.20816 0.207607
;;
;; Three trials with new undo-make-selective-list code:
;;   0.072031 0.072052 0.071874

(require 'cl-lib)

;; Determine number of iterations necessary to trunate "aaa" from undo history
(defun play-num-iters ()
  (interactive)
  (save-excursion
    (goto-char 141)
    (insert "aaa")
    (undo-boundary)
    (undo)
    (undo-boundary))
  (let ((iters 0))
    (insert "b")
    (undo-boundary)
    (while (cl-find "aaa"
                    buffer-undo-list
                    :test
                    (lambda (aaa rhs)
                      (and (listp rhs)
                           (stringp (car rhs))
                           (string= aaa (car rhs)))))
      (undo) ; delete "b"
      (undo-boundary)
      (undo) ; reinsert "b"
      (undo-boundary)
      (garbage-collect)
      (cl-incf iters))
    (message "iters=%s" iters)))

(defun play-insert ()
  (interactive)
  (save-excursion
    (goto-char 141)
    (insert "aaa")
    (undo-boundary)
    (undo)
    (undo-boundary))
  (let ((iters 0))
    (insert "b")
    (undo-boundary)
    ;; 499 is (- (play-num-iters) 1)
    (while (< iters 499)
      (undo) ; delete "b"
      (undo-boundary)
      (undo) ; reinsert "b"
      (undo-boundary)
      (cl-incf iters))
    (garbage-collect)))

