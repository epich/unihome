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

;; TODO: Implement coloring of mismatched parens

;; TODO: Write tests:
;;
;;   ;; (abc ...) is consistent, (def ...) is inconsistent in the following:
;;   (abc a-symbol (a-func-call "word_a
;;   word_b" (def ghi
;;           jkl)

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

(defface cp-inconsistent
  '((((class color) (background light))
     :foreground "dark orange")
    (((class color) (background dark))
     :foreground "orange"))
  "Face to use for matching open and close parens whose placement
is inconsistent with indentation."
  :group 'color-parens-faces)

(defcustom cp-inconsistency-max-lines 1000
  "The number of lines across which color-parens will detect
  inconsistencies between indentation and the placement of close
  parens. This is meant to tradeoff accuracy across long buffer
  distances for sake of performance."
  :group 'color-parens
  :type 'integer)

;; An open paren and algorithmic data about it. Instances are placed
;; on a stack as this packages parses a buffer region.
;;
;; position is the position in the buffer of the open paren
;;
;; close is one of:
;;   - the position before the matching close paren
;;   - the symbol "mismatched" if no matching close paren exists (TODO)
;;   - nil if unknown
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

(defsubst cp--line-check-opens (open-stack)
  "Check cp--Open objects of the OPEN-STACK list for
consistency.

The inconsistent==nil elements of OPEN-STACK must have columns
that are strictly decreasing moving towards the tail (a necessary
but not sufficient condition for being consistent). The
implementation optimizes on this assumption.

Call with point on the line being checked; puts point on the next
line or EOB."
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

(defsubst cp--region-check-opens (downward-objs
                                  upward-objs
                                  beg-limit
                                  end-limit)
  "Propertize inputted parens in a region, first going down in
sexp depth then up per the DOWNWARD-OBJS and UPWARD-OBJS.

Point must be at the start of the region to process and will end
up near the end.

DOWNWARD-OBJS is a list of cp--Open objects. Each must be a
parent of the next in the list.

UPWARD-OBJS is a list of cp--Open objects. Each must be a child
of the next in the list.

BEG-LIMIT and END-LIMIT are buffer positions limiting the portion
of the buffer scanned."
  ;; TODO: Pop from downward-objs and upward-objs based on limits
  (while downward-objs
    (cp--line-check-opens upward-objs)
    (while (and downward-objs
                (< (cp--Open-position (car downward-objs))
                   (point)))
      (push (pop downward-objs)
            upward-objs)))
  (while (and upward-objs
              (cp--Open-close (car upward-objs)))
    (cp--line-check-opens upward-objs)
    (while (and upward-objs
                (< (cp--Open-close (car upward-objs))
                   (point)))
      (pop upward-objs))))

(defsubst cp--set-closes (open-obj-list init-pos)
  "Sets the close attribute of each element of OPEN-OBJ-LIST.

OPEN-OBJ-LIST is a list of cp--Open. Each must be a child of the
next in the list. This is used to scan-lists efficiently.

INIT-POS is a buffer position that is an immediate child of the
first element of OPEN-OBJ-LIST. In other words: up-list from
INIT-POS should arrive at its close position."
  (let ((buf-pos init-pos))
    (dolist (open-i open-obj-list)
      (when buf-pos
        (setq buf-pos (condition-case nil
                          (scan-lists buf-pos 1 1)
                        (scan-error nil))))
      (setf (cp--Open-close open-i) (if buf-pos
                                        (1- buf-pos)
                                      ;; TODO: Set to 'mismatched
                                      nil)))))

(defun cp-propertize-region (start end)
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
        (cp--set-closes open-objs start)
        ;; Check lists beginning before JIT lock's region (could
        ;; scan to after JIT lock's region)
        (let ((open-objs-reversed (reverse open-objs)))
          ;; Position to goto is non nil because ps-opens is non nil
          (goto-char (cp--Open-position (car open-objs-reversed)))
          (cp--region-check-opens open-objs-reversed
                                  nil
                                  (point-min)
                                  (point-max))))
      (goto-char start)
      (push (current-time) timing-info)
      (let* (;; Sparse vector of open paren data, indexed by position in
             ;; buffer minus start. The purpose is speed.
             (open-paren-table (make-vector (- end start) nil)))
        (push (current-time) timing-info)
        (while (< (point) end)
          (let ((indent-pos (progn (back-to-indentation)
                                   (point)))
                ;; Column at which text starts on the line, except if
                ;; inside a string. Text doesn't start in a comment,
                ;; since ; is text.
                (indent-column (current-column))
                (line-ppss (syntax-ppss))
                (line-end (progn (end-of-line)
                                 (point))))
            ;; Skip whitespace only lines and lines beginning inside
            ;; string
            (unless (or (eq indent-pos line-end)
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
                                               :column (progn
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
              (push (or (aref open-paren-table
                              (- ps-open-i start))
                        (progn
                          (push (make-cp--Open
                                 :position ps-open-i
                                 :column (progn
                                           (goto-char ps-open-i)
                                           (current-column)))
                                open-objs)
                          (aset open-paren-table
                                (- ps-open-i start)
                                (car open-objs))))
                    open-obj-list)))
          (cp--set-closes open-obj-list end)
          (goto-char end)
          ;; Check lists beginning in JIT lock's region but ending
          ;; after it.
          (cp--region-check-opens nil
                                  open-obj-list
                                  (point-min)
                                  (point-max)))
        (push (current-time) timing-info)
        (dolist (open-i open-objs)
          ;; Set close position
          ;;
          ;; Note: We do it here instead of when it was made so as
          ;; some benefit from the cp--set-closes function's buffer
          ;; scanning optimization. The lists processed here are
          ;; opened and closed within JIT lock's region, so the less
          ;; efficient buffer scanning is not a big deal.
          (unless (cp--Open-close open-i)
            (setf (cp--Open-close open-i)
                  (condition-case nil
                      (1- (scan-lists (cp--Open-position open-i) 1 0))
                    ;; TODO: Set to 'mismatched
                    (scan-error nil))))
          (if (cp--Open-inconsistent open-i)
              (color-parens--colorize (list (cp--Open-position open-i)
                                            (cp--Open-close open-i))
                                      'cp-inconsistent)
            (color-parens--decolorize (list (cp--Open-position open-i)
                                            (cp--Open-close open-i)))))
        (push (current-time) timing-info)
        (my-msg "DEBUG: cp-color-parens start=%s end=%s timing: %s"
                start end
                (my-time-diffs (nreverse timing-info)))
        ))))

(defun color-parens-unpropertize-region (start end)
  ;; TODO: remove-text-properties
  )

(defsubst color-parens-extend-region-after-change (start end _old-len)
  ;; It seems redisplay works its way from before start to after end,
  ;; so it's more important to expand the start in order to get
  ;; correct redisplays.
  ;; TODO: Account for cp-inconsistency-max-lines
  (setq jit-lock-start (or (syntax-ppss-toplevel-pos (syntax-ppss start))
                           start)))

(define-minor-mode color-parens-mode
  "Color unbalanced parentheses and parentheses inconsistent with
  indentation."
  nil nil nil
  (if color-parens-mode
      (progn
        (jit-lock-register 'cp-propertize-region t)
        (add-hook 'jit-lock-after-change-extend-region-functions
                  'color-parens-extend-region-after-change
                  nil
                  t))
    (jit-lock-unregister 'cp-propertize-region)
    (color-parens-unpropertize-region (point-min) (point-max))))

(provide 'color-parens)

;;; color-parens.el ends here
