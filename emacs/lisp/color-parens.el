;;; color-parens.el --- Color unbalanced parentheses and parentheses inconsistent with indentation -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Barry O'Reilly <gundaetiapo@gmail.com>
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO: Options for performance improvement:
;; : c-beginning-of-statement-1 is fontified in full 48 times
;;   : Do what font-lock-fontify-region does to mark text fontified
;;     : Will this prevent other jit-lock-functions including font-lock-fontify-region from doing their bit?
;; : Skip more processing when outside JIT lock's region
;; : Don't use parse-partial-sexp in one char increments
;; : Instead of expanding the region JIT lock passes, process the
;;   extended region minimally: sexp parsing is unnecessary. Parse
;;   line by line comparing (current-column) to the subset of open
;;   parens which extend into the region to prove they are consistent.
;; : Instead of expanding the region after each buffer change, perhaps
;;   just call redisplay ourselves. Maybe better overall.

;; TODO: Threshold column for `() is off.
;;
;; Consistent:
;;   `()
;;    foo
;;
;; Inconsistent:
;;   `()
;;   foo
;;
;; (But font lock is on the open paren, not the backtick)
;;
;; Similarly, ,@() is off by two

;; TODO: Algorithm doesn't account for:
;;
;; (abc
;;   (def))
;;  (ghi)
;;
;; (abc ...) are inconsistent parens because (ghi) is indented too far

;; TODO: How to handle:
;;
;; (abc a-symbol (a-func-call "word_a
;; word_b" (def ghi
;;         jkl)
;;
;; (abc a-symbol (a-func-call "word_a
;; word_b" (def)
;;                            jkl))
;;
;; And the inputted region is only the jkl lines.
;;
;; Probably doesn't matter significantly, as long as it's consistent
;; regardless of how JIT inputs the regions.

;; TODO: Implement coloring of parens with no pair at all

;;; Code:

(require 'cl-lib)
(require 'my-util) ; TODO

(defgroup color-parens nil
  "Color unbalanced parentheses and parentheses inconsistent with indentation."
  :prefix "color-parens-"
  :group 'paren-matching)

(defgroup color-parens-faces nil
  "Faces for color-parens package. "
  :group 'color-parens
  :group 'faces)

(defface color-parens-inconsistent
  '((((class color) (background light))
     :foreground "dark orange")
    (((class color) (background dark))
     :foreground "orange"))
  "Face to use for matching open and close parens whose placement
is inconsistent with indentation."
  :group 'color-parens-faces)

;; An open paren and algorithmic data about it. Instances are placed
;; on a stack as this packages parses a buffer region.
;;
;; position is the position in the buffer of the open paren
;;
;; close is the position before the closing paren, or nil if unknown
;;
;; column is the displayed column of the open paren in its logical
;; line of the buffer
;;
;; inconsistent is whether the open paren's close paren is inconsistent
;; with the indentation within the list defined by the parens.
;;
;;   nil means unknown
;;
;;   t means inconsistent
;;
;; NB: There's no value for "consistent" because once it is known, the
;; struct instance is popped and no longer used.
(cl-defstruct cp--Open position close column inconsistent)

(defsubst color-parens--colorize (positions face-arg)
  "Colorize chars in the buffer to the specified FACE-ARG with
Font Lock.

POSITIONS is a list of positions in the buffer to colorize. nil
positions are ignored."
  (with-silent-modifications
    (dolist (pos-i positions)
      (when pos-i
        (add-text-properties pos-i
                             (1+ pos-i)
                             `(font-lock-face
                               ,face-arg
                               rear-nonsticky
                               t))))))

(defsubst color-parens--decolorize (positions)
  "Decolorize chars in the buffer colored with Font Lock.

POSITIONS is a list of positions in the buffer to colorize."
  (with-silent-modifications
    (dolist (pos-i positions)
      (when pos-i
        (remove-text-properties pos-i
                                (1+ pos-i)
                                '(font-lock-face
                                  nil
                                  rear-nonsticky
                                  nil))))))

(defsubst color-parens--update-inconsistency-colors (inconsistentp
                                                     open-paren
                                                     close-paren)
  "Update inconsistency Font Lock colors for OPEN-PAREN and
CLOSE-PAREN as buffer positions based on INCONSISTENTP."
  (if inconsistentp
      (color-parens--colorize (list open-paren close-paren)
                              'color-parens-inconsistent)
    (color-parens--decolorize (list open-paren close-paren))))

(defun cp-propertize-region-3 (start end)
  (save-excursion
    (goto-char start)
    (let ((paren-stack nil)
          (line-ppss (syntax-ppss)))
      (dolist (open-pos (nth 9 line-ppss))
        ;; TODO: Initialize :inconsistent
        (push (make-cp--Open :position open-pos
                                       :column (save-excursion
                                                 (goto-char open-pos)
                                                 (current-column)))
              paren-stack))
      (while (< (point) end)
        (let (;; Column at which text starts on the line, except if
              ;; inside a string. Text doesn't start in a comment,
              ;; since ; is text.
              (text-column (progn (back-to-indentation)
                                  (current-column)))
              (line-end (save-excursion (end-of-line)
                                        (point))))
          ;; Skip whitespace only lines and lines beginning inside
          ;; string
          (unless (or (eq (point) line-end)
                      (nth 3 line-ppss)) ; Whether inside string
            ;; Mark open parens on the paren-stack that become
            ;; inconsistent because of the current line.
            (let ((open-i paren-stack))
              ;; If one considers only the inconsistent==nil Opens on
              ;; the paren-stack, their columns are strictly
              ;; decreasing moving down the stack (towards the tail).
              ;; Since we're only interested in marking Opens
              ;; inconsistent, that allows the iteration to stop at
              ;; the first inconsistent==nil Open with small enough
              ;; column.
              (while (and open-i
                          (or (cp--Open-inconsistent (car open-i))
                              (<= text-column
                                  (cp--Open-column (car open-i)))))
                (setf (cp--Open-inconsistent (car open-i))
                      t)
                (pop open-i))))
          ;; TODO: Let bound current point and sexp depth here so as
          ;; scan-lists can use it to efficiently find the close
          ;; parens we're passing over.
          ;;
          ;; Go to next line. Since we already know line-end, use it
          ;; instead of rescanning the line
          (goto-char (min (1+ line-end) (point-max)))
          (setq line-ppss (syntax-ppss))
          (let* ((open-positions (nth 9 line-ppss))
                 (common-open (and paren-stack
                                  (let ((cons-i open-positions))
                                    (while (and (cdr cons-i)
                                                (<= (cadr cons-i)
                                                    (cp--Open-position (car paren-stack))))
                                      (pop cons-i))
                                    cons-i))))
            ;; Process parens that closed upon going to this next line
            (while (and paren-stack
                        (or (not common-open)
                            (/= (cp--Open-position (car paren-stack))
                                (car common-open))))
              (let ((close-pos (condition-case nil
                                   (1- (scan-lists (cp--Open-position (car paren-stack))
                                                   1 0))
                                 (scan-error nil)))
                    (open-obj (pop paren-stack)))
                ;; TODO: Set mismatched paren face if close-pos is nil
                ;; TODO: If operating on a subset region, it's not
                ;; always correct to color consistent
                (color-parens--update-inconsistency-colors
                 (cp--Open-inconsistent open-obj)
                 (cp--Open-position open-obj)
                 close-pos)))
            ;; Create new cp--Open objects
            (save-excursion
              (dolist (open-i (if common-open
                                  (cdr common-open)
                                open-positions))
                (goto-char open-i)
                ;; TODO: Account for the case of skipping lines that
                ;; begin in a string. In those cases, these parens
                ;; we're adding could be inconsistent amongst
                ;; themselves in a way that the algorithm won't detect
                ;; because of the short circuiting of the paren-stack
                ;; check. Either do a particular consistency check
                ;; here, or add cp--Open objects on lines
                ;; that begin in a string (leaning towards former).
                (push (make-cp--Open :position open-i
                                               :column (current-column))
                      paren-stack))))))
      (dolist (open-i paren-stack)
        (color-parens--update-inconsistency-colors
         (cp--Open-inconsistent open-i)
         (cp--Open-position open-i)
         (condition-case nil
             (1- (scan-lists (cp--Open-position open-i)
                             1 0))
           (scan-error nil)))))))

(defsubst cp--line-check-opens (open-stack)
  """Check cp--Open objects of the OPEN-STACK list for
consistency.

The inconsistent==nil elements of OPEN-STACK must have columns
that are strictly decreasing moving towards the tail. Otherwise
they would be inconsistent. The implementation optimizes on this
assumption.

Call with point on the line being checked; puts point on the next
line or EOB."""
  (let ((indent-pos (progn (back-to-indentation)
                           (point)))
        (indent-column (current-column))
        (line-end (progn (end-of-line)
                         (point))))
    ;; Assess open-objs against indent-column
    (unless (eq indent-pos line-end) ; Skip whitespace lines
      ;; Since we're only interested in marking Opens inconsistent,
      ;; the open-stack's documented property allows the iteration to
      ;; stop at the first inconsistent==nil Open with small enough
      ;; column.
      (while (and open-stack
                  (or (cp--Open-inconsistent (car open-stack))
                      (<= indent-column
                          (cp--Open-column (car open-stack)))))
        ;; Check cp--Open-inconsistent to avoid excessive
        ;; syntax-ppss when there's a lot of bad
        ;; indentation.
        (unless (or (cp--Open-inconsistent (car open-stack))
                    ;; Multi line strings don't cause inconsistency
                    (nth 3 (syntax-ppss indent-pos)))
          (setf (cp--Open-inconsistent (car open-stack))
                t))
        (pop open-stack)))
    ;; Go to next line. Since we already know line-end, use it
    ;; instead of rescanning the line
    ;;
    ;; TODO: Better account for EOB, mark remaining
    ;; open-objs mismatched as appropriate.
    (goto-char (min (1+ line-end) (point-max)))))

;; TODO: Address duplication: Between broad scan before region and after region
;; TODO: Create function just for passing a list of cp--Open to find close position if nil and colorize, the three parts of this function would call it.
(defun cp-propertize-region-2 (start end)
  (save-excursion
    (let* ((timing-info (list (current-time)))
           (start-ps (syntax-ppss start))
           ;; Open positions, outer to inner
           (ps-opens (nth 9 start-ps))
           ;; cp--Open objects, positions inner to outer
           (open-objs nil))
      (push (current-time) timing-info)
      ;; Process the broader region spanned by ps-opens. We need only
      ;; check the ps-opens themselves, not their children lying
      ;; outside the region.
      ;;
      ;; Efficiency is important because of the broad region covered.
      ;; Sexp parsing is mostly avoided, except to check whether a
      ;; line began with a multi line string as the last check before
      ;; marking an open paren inconsistent.
      (when ps-opens
        ;; Initialize cp--Open objects
        (dolist (ps-open-i ps-opens)
          (push (make-cp--Open :position
                               ps-open-i
                               :column
                               (progn
                                 (goto-char ps-open-i)
                                 (current-column)))
                open-objs))
        ;; Initialize close positions of cp--Open objects
        (let ((open-objs-reversed nil))
          (dolist (open-i open-objs)
            ;; TODO: short circuit dolist if scan-error
            (let ((close-pos (condition-case nil
                                 (scan-lists (cp--Open-position open-i) 1 0)
                               (scan-error nil))))
              (when close-pos
                (goto-char close-pos)
                (setf (cp--Open-close open-i) (1- close-pos))))
            (push open-i open-objs-reversed))
          (goto-char (cp--Open-position (car open-objs-reversed)))
          (let ((downward-i open-objs-reversed)
                (upward-i nil))
            (while downward-i
              (cp--line-check-opens upward-i)
              (while (and downward-i
                          (< (cp--Open-position (car downward-i))
                             (point)))
                (push (pop downward-i)
                      upward-i)))
            (while (and upward-i
                        (cp--Open-close (car upward-i)))
              (cp--line-check-opens upward-i)
              (while (and upward-i
                          (< (cp--Open-close (car upward-i))
                             (point)))
                (pop upward-i))))))
      (goto-char start)
      (push (current-time) timing-info)
      (let* (;; Sparse vector of open paren data, indexed by position in
             ;; buffer minus start. The purpose is speed.
             (open-paren-table (make-vector (- end start) nil)))
        (push (current-time) timing-info)
        (while (< (point) end)
          (let (;; Column at which text starts on the line, except if
                ;; inside a string. Text doesn't start in a comment,
                ;; since ; is text.
                (indent-column (progn (back-to-indentation)
                                      (current-column)))
                (line-ppss (syntax-ppss))
                (line-end (save-excursion (end-of-line)
                                          (point))))
            ;; Skip whitespace only lines and lines beginning inside
            ;; string
            (unless (or (eq (point) line-end)
                        (nth 3 line-ppss))
              ;; Iterate over list of unclosed open parens
              (dolist (open-pos (nth 9 line-ppss))
                ;; Skip the already processed ones outside the region
                (when (<= start open-pos)
                  (let ((open-obj (or (aref open-paren-table
                                            (- open-pos start))
                                      (progn
                                        (push (make-cp--Open
                                               :position open-pos
                                               :column (save-excursion
                                                         (goto-char open-pos)
                                                         (current-column)))
                                              open-objs)
                                        (aset open-paren-table
                                              (- open-pos start)
                                              (car open-objs))))))
                    (when (<= indent-column
                              (cp--Open-column open-obj))
                      (setf (cp--Open-inconsistent open-obj)
                            t))))))
            ;; Go to next line. Since we already know line-end, use it
            ;; instead of rescanning the line
            (goto-char (min (1+ line-end) (point-max)))))
        (push (current-time) timing-info)
        (let ((ps-opens (nth 9 (syntax-ppss end)))
              ;; Inner to outer going towards the tail
              (open-obj-list nil))
          (dolist (ps-open-i ps-opens)
            (when (<= start ps-open-i)
              (let ((open-i (or (aref open-paren-table
                                      (- ps-open-i start))
                                ;; TODO: Duplicates above
                                (progn
                                  (push (make-cp--Open
                                         :position ps-open-i
                                         :column (save-excursion
                                                   (goto-char ps-open-i)
                                                   (current-column)))
                                        open-objs)
                                  (aset open-paren-table
                                        (- ps-open-i start)
                                        (car open-objs))))))
                ;; TODO: short circuit dolist if scan-error
                (let ((close-pos (condition-case nil
                                     (scan-lists (cp--Open-position open-i) 1 0)
                                   (scan-error nil))))
                  (when close-pos
                    (goto-char close-pos)
                    (setf (cp--Open-close open-i) (1- close-pos))))
                (push open-i open-obj-list))))
          (goto-char end)
          (let ((upward-i open-obj-list))
            (while (and upward-i
                        (cp--Open-close (car upward-i)))
              (cp--line-check-opens upward-i)
              (while (and upward-i
                          (< (cp--Open-close (car upward-i))
                             (point)))
                (pop upward-i)))))
        (push (current-time) timing-info)
        (dolist (open-i open-objs)
          ;; Set close position
          (unless (cp--Open-close open-i)
            (setf (cp--Open-close open-i)
                  (condition-case nil
                      (1- (scan-lists (cp--Open-position open-i) 1 0))
                    (scan-error nil))))
          (if (cp--Open-inconsistent open-i)
              (color-parens--colorize (list (cp--Open-position open-i)
                                            (cp--Open-close open-i))
                                      'color-parens-inconsistent)
            ;; TODO: Until we process parens after end of region, check
            ;; its within
            (when (< (cp--Open-close open-i) end)
              (color-parens--decolorize (list (cp--Open-position open-i)
                                              (cp--Open-close open-i))))))
        (push (current-time) timing-info)
        (my-msg "DEBUG: cp-color-parens start=%s end=%s timing: %s"
                start end
                (my-time-diffs (nreverse timing-info)))
        ))))

(defun cp-propertize-region-1 (start end)
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (let (;; Push at open parens, pop at close parens
          (paren-stack)
          (parse-state (syntax-ppss)))
      (while (< (point) end)
        (let (;; Column at which text starts on the line, except if
              ;; inside a string. Text doesn't start in a comment,
              ;; since ; is text.
              (text-column (progn (back-to-indentation)
                                  (current-column)))
              (line-end (save-excursion (end-of-line)
                                        (point))))
          ;; Skip whitespace only lines
          (unless (eq (point) line-end)
            (unless (nth 3 parse-state) ; Whether inside string
              ;; Mark open parens on the paren-stack that become
              ;; inconsistent because of the current line.
              (let ((open-i paren-stack))
                ;; If one considers only the inconsistent==nil Opens on
                ;; the paren-stack, their columns are strictly
                ;; decreasing moving down the stack (towards the tail).
                ;; Since we're only interested in marking Opens
                ;; inconsistent, that allows the iteration to stop at
                ;; the first inconsistent==nil Open with small enough
                ;; column.
                (while (and open-i
                            (or (cp--Open-inconsistent (car open-i))
                                (<= text-column
                                    (or (cp--Open-column (car open-i))
                                        ;; Lazy computation of column
                                        (save-excursion
                                          (goto-char (cp--Open-position (car open-i)))
                                          (setf (cp--Open-column (car open-i)) (current-column)))))))
                  (setf (cp--Open-inconsistent (car open-i))
                        t)
                  (pop open-i))))
            ;; Note: point is at indentation
            (while (and (< (point) line-end))
              (let ((depth-change
                     (- (car parse-state)
                        (car (setq parse-state
                                   ;; TODO: parsing 1 char at a time
                                   ;; is too slow.
                                   ;; (parse-partial-sexp (point)
                                   ;;                     (1+ (point))
                                   ;;                     nil
                                   ;;                     nil
                                   ;;                     parse-state
                                   ;;                     nil)
                                   ;; Improves speed with 666666 hack
                                   ;; (stop at any depth change)
                                   (parse-partial-sexp (point)
                                                       line-end
                                                       666666
                                                       nil
                                                       parse-state
                                                       'syntax-table)
                                   )))))
                (cond
                 ((or (= 0 depth-change)   ; Didn't cross a paren
                      (nth 3 parse-state)  ; Inside a string
                      (nth 4 parse-state)) ; Inside a comment
                  nil) ; Keep parsing
                 ;; Case: stopped at open paren
                 ((< depth-change 0)
                  ;; Note: cp--Open's column field is
                  ;; initialized nil and computed lazily. This avoids
                  ;; computing current-column for open parens closed
                  ;; on the same line. These also tend to be further
                  ;; from the beginning of line.
                  (push (make-cp--Open :position (1- (point)))
                        paren-stack))
                 ;; Case: stopped at close paren
                 ((< 0 depth-change)
                  (if paren-stack
                      (progn
                        (color-parens--update-inconsistency-colors
                         (cp--Open-inconsistent (car paren-stack))
                         (cp--Open-position (car paren-stack))
                         (1- (point)))
                        (pop paren-stack))
                    ;; TODO: Handle close paren when nil paren-stack
                    ))))))
          ;; Go forward to beginning of next line, keeping parse-state
          ;; up to date
          (unless (eobp)
            (setq parse-state
                  (parse-partial-sexp (point)
                                      (1+ (point))
                                      nil
                                      nil
                                      parse-state))))))))

(defun color-parens-unpropertize-region (start end)
  ;; TODO: remove-text-properties
  )

(defun color-parens-extend-region (start end)
  "Extend region for JIT lock to fontify."
  (save-excursion
    (list (or (syntax-ppss-toplevel-pos (syntax-ppss start))
              start)
          (let ((last-top-level (syntax-ppss-toplevel-pos (syntax-ppss end))))
            (if last-top-level
                (progn
                  (goto-char last-top-level)
                  (forward-sexp)
                  (point))
              end)))))

;; TODO: Extend to either outermost list or visible range? Consider
;; how the code will know it can change inconsistent to consistent if
;; not processing an expanded region.
(defsubst color-parens-extend-region-after-change (start end _old-len)
  (let ((extended-region (color-parens-extend-region start end)))
    (setq jit-lock-start (car extended-region))
    (setq jit-lock-end (cadr extended-region))))

(defalias 'cp-propertize-region 'cp-propertize-region-2)

(define-minor-mode color-parens-mode
  "Color unbalanced parentheses and parentheses inconsistent with
  indentation."
  nil nil nil
  (if color-parens-mode
      (progn
        (jit-lock-register 'cp-propertize-region
                           t)
        ;; TODO: Expand region or not?
        ;; (add-hook 'jit-lock-after-change-extend-region-functions
        ;;           'color-parens-extend-region-after-change
        ;;           nil
        ;;           t)
        )
    (jit-lock-unregister 'cp-propertize-region)
    (color-parens-unpropertize-region (point-min) (point-max))))

(provide 'color-parens)

;;; color-parens.el ends here
