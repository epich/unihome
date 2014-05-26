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

;; TODO: Problems undo-redo-table solves:
;;   - undo in region adjustments
;;   - undo-only to skip over undos in region
;;   - undo-only in region to work
;;   - repeated undos and redos of large deletions can share deleted
;;     Lisp_String
;;   - Undo Tree integration (with additional change to disambiguate
;;     regional from nonregional, perhaps by mapping the nil boundary
;;     that closes a change group.

;; undo-deltas:
;; TODO: Should take the form (UNDONE POS . OFFSET) where
;; UNDONE is at first a reference to cons of buffer-undo-list,
;; but later its the resulting undo-delta. Later still in
;; undo-adjust-pos, it is temporarily swapped with a
;; subadjustment to account for the "ddd" problem.

;; Outline for "A plan to integrate Undo Tree into Emacs"
;;   - Describe undo tree's current implementation
;;     - Reference Toby's post http://lists.gnu.org/archive/html/emacs-devel/2010-05/msg00036.html
;;     - copies from buffer-undo-list
;;     - plays tricks with marker adjustments
;;   - Undo Tree and builtin undo to use buffer-undo-list and undo-redo-table as a common data model
;;   - Describe how to interpret builtin undo commands in the context of an undo tree
;;     - undo in region as navigating to a parallel tree
;;       - undo-tree does not exactly model it this way
;;     - undo-only as "go to the parent"
;;     - Run of undo means "retrace my edge traversals"
;;       - Breaking the run of undo means you have to retrace *those* edge traversals
;;     - In builtin undo system, to reach prior buffer states you either "go to parent" or "retrace edge traversals"
;;       - But Undo Tree allows one to reach prior buffer states more intuitively and more capably
;;   - Undo Tree as default
;;   - Mention undo tree's node diff feature
;;   - Mention undo tree's history persistence
;;   - Mention undo tree may need to push to buffer-undo-list representations of its more flexible edge traversal and tree navigation
;;   - Edge traversals cause undo history to grow
;;     - Mention my idea for reducing growth of history
;;     - Mention Stefan's idea of eliminating history growth altogether
;;   - In response to Toby's question about compact_undo_list:
;;     - compact_undo_list looks more like an opportunistic optimization
;;       - Perhaps motivated by some algorithms having time complexity proportional to number of markers
;;     - Maybe instead of splicing out unmarked markers, we should splice out markers pointing to nowhere
;;       - Wouldn't necessarily have to be done as part of GC
;;       - If a few parts of Emacs forget to point a marker to nowhere, not critical to correctness of undo list
;;   - Problem of undo tree's modeling on top of builtin undo system
;;     - Maybe:
;;       - hash table with weak key mapping undo change groups to tree node
;;       - another hash table with weak value, mapping tree node to its most recent change group
;;       - timer or GC hook to prune tree nodes which are no longer in latter hash table
;;     - Or maybe implement general weak references for Elisp
