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

;; Outline for "Integration of Undo Tree in Emacs"
;;   - Describe undo tree's current implementation
;;     - Reference Toby's post http://lists.gnu.org/archive/html/emacs-devel/2010-05/msg00036.html
;;     - copies from buffer-undo-list
;;     - plays tricks with marker adjustments
;;   - Undo Tree and builtin undo to use buffer-undo-list and undo-redo-table such that user can perhaps use them interchangably when undo-tree-mode enabled
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
;;       - hash table undo-tree-node-table with weak key mapping undo change groups to tree node
;;       - another hash table with weak value, mapping tree node to its most recent change group
;;       - timer or GC hook to prune tree nodes which are no longer in latter hash table
;;     - Or maybe implement general weak references for Elisp
;;     - Or: define undo-tree-node-table, whenever new nodes are created, also scan through buffer-undo-list to mark (via a keep boolean in a undo-tree-node struct) nodes, and then sweep
;;       - Do this in after-change-functions
;;       - Undo Tree only prunes the undo tree during an interactive command, which is also when new nodes are created
;;     - Or: use a post-gc-hook to cleanup the tree
;;   - Mention my plan: finish bug 16411, may or may not dive into Undo Tree
;;
;; I believe it is desirable to integrate Undo Tree into Emacs, and I
;; have some ideas about doing so that I hope Toby and the community
;; will have some feedback about.

Cutting undo/redo pairs and Undo Tree integration discussion

Stefan wrote at
http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16411#77 :

> Yes. And I think that's what we want: as a user, having to wade
> through N repetitions of redo/undo is a pain. Those that suffer
> often from it probably switched to undo-tree.

If one conceives of undo history as a tree, then a run of undo
commands means "retrace my edge traversals". Then if you break that
run, you have to retrace *those* edge traversals.

undo-only means "go to the parent".

I prefer to think of undo-in-region as navigating to a parallel tree
that looks much like the previous, but rejoins it at the node before
the oldest change group undo-in-region drew from. This conception
means each tree node is a buffer state. Undo Tree doesn't model
undo-in-region quite like that, but close: it visually copies that
"parallel tree" and roots it similarly. For some reason the new tree
fragment misses branches, but that might be a bug.

The appeal of Undo Tree is that it allows the user to more intuitively
and directly navigate the underlying tree. While the builtin "classic"
undo system allows navigation by "retrace my edge traversals" and "go
to the parent", Undo Tree allows "go to any child", "go to the
parent", and "select an arbitrary node" (using
undo-tree-visualizer-selection-mode).

Lately I'm using the builtin undo system partly to eat my own dogfood
and partly because builtin undo-in-region now works better. But I do
miss Undo Tree's better capability to revisit prior buffer states. The
ability to diff nodes is quite handy too.

After I finish bug 16411, I'm considering moving on to Undo Tree and
working it to better coexist with the classic undo system. Since the
classic commands can be thought of in tree terms, I believe we can
work towards integrating the two in Emacs.

To address your original point, my thinking was that if Undo Tree
coexists with the classic system, then merely traversing edges in the
undo tree would cause undo history to grow. I think this would be
tolerable if growth is small. To me, cutting the extra undo/redo pairs
as you suggest means abandoning the classic system in favor of
something like Undo Tree. That is perfectly fine by me and it might be
relatively easier too.

While we're on the subject, I'd like to reply to some of Toby's
thoughts in his 2009 post at
http://lists.gnu.org/archive/html/emacs-devel/2010-05/msg00036.html
and add some of my own.
