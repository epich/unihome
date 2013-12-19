;;; color-parens.el --- Color unbalanced parentheses and parentheses inconsistent with indentation -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Barry O'Reilly <gundaetiapo@gmail.com>
;; Version: 0.1

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

;; Colors open and close parentheses which are inconsistent with the
;; indentation of lines between them. This is useful for the Lisp
;; programmer who infers a close paren's location from the open paren
;; and indentation. The coloring serves as a warning that the
;; indentation misleads about where the close paren is. It may also
;; help to localize the mistake, whether due to a misindented line or
;; a misplaced paren.
;;
;; As an example, consider:
;;
;;   (aaa (bbb "word-a
;;   word-b" (ccc 1
;;                2)
;;        fff))
;;
;; (aaa ...) and (ccc ...) are consistent, so are not colored.
;; (bbb ...) is inconsistent because the indentation of fff is
;; inconsistent with the actual location of the close paren. The open
;; and close paren are thus colored with the cp-inconsistent face.
;; This example also shows that multi line strings don't cause an
;; inconsistency.
;;
;; Currently, the package only detects close parens that are after the
;; place indentation would predict. A planned feature is to also
;; indicate when the close paren is before.
;;
;; Also planned is to show mismatched parens.

;;; Code:

;; TODO: Algorithm doesn't account for close paren which is too soon.
;;
;; (abc
;;   (def))
;;   (ghi)
;;
;; (abc ...) are inconsistent parens because (ghi) is indented too far

;; TODO: Implement coloring of mismatched parens

;; TODO: Bug:
;;
;;   (when (and t
;;         nil))
;;
;; (and ...) is colored inconsistent correctly, but then add close paren:
;;
;;   (when (and t)
;;         nil))
;;
;; (and ...)'s open remains colored incorrectly, but the close was cleared.
;;
;; A problematic variation: '(a (b)' (a ...) inconsistent, but delete
;; so as '(a b)'.
;;
;; Another problematic variation with close parens: '(a ...)'
;; inconsistent, edit so as: '(a ...()', close paren remains
;; inconsistent incorrectly.
;;
;; Possible solution to these: Write a function to clear the text
;; property from:
;;
;;   - open parens in the JIT lock region
;;   - close parens in the JIT lock region and whose open is also*
;;
;; (Couldn't think of a case where the text property needs clearing
;; outside the JIT lock region.)
;;
;; * Determining whether a close's open is in the region could be
;; inefficient. An alternative is to clear these text properties in
;; the JIT lock region first, then when when processing the broader
;; region, refontify the close parens based on the open parens.
;;
;; Also use the new function when turning minor mode off.
;;
;; (skip-syntax-forward "^()" end) looks useful

;; TODO: Write tests:
;;
;;   ;; (abc ...) is consistent, (def ...) is inconsistent in the following:
;;   (abc a-symbol (a-func-call "word-a
;;   word-b" (def ghi
;;           jkl)

(require 'cl-lib)

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

;; An open paren and algorithmic data about it.
;;
;; position is the position in the buffer of the open paren
;;
;; close is one of:
;;   - nil if unknown
;;   - the position before the matching close paren
;;   - the symbol 'mismatched if no matching close paren exists (TODO)
;;
;; column is the displayed column of the open paren in its logical
;; line of the buffer
;;
;; inconsistent is whether the open paren's close paren is
;; inconsistent with the indentation within the list defined by the
;; parens. It is one of:
;;   - nil if unknown or consistent
;;   - an integer offset from the open position to the position of the
;;     first inconsistency. This offset is also cached in the open
;;     paren text properties for performance.
(cl-defstruct cp--Open position close column inconsistent)

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
                (- indent-pos (cp--Open-position (car open-stack)))))
        (pop open-stack)))
    ;; Go to next line. Since we already know line-end, use it
    ;; instead of rescanning the line
    ;;
    ;; goto-char tolerates going beyond EOB
    (goto-char (1+ line-end))))

(defsubst cp--region-check-opens (downward-objs
                                  upward-objs)
  "Check inputted parens in a region for inconsistency, first
going down in sexp depth then up per the DOWNWARD-OBJS and
UPWARD-OBJS.

Point must be at the start of the region to process and will end
up near the end.

DOWNWARD-OBJS is a list of cp--Open objects. Each must be a
parent of the next in the list.

UPWARD-OBJS is a list of cp--Open objects. Each must be a child
of the next in the list."
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

(defsubst cp--set-closes (open-obj-list)
  "Sets the close attribute of each element of OPEN-OBJ-LIST.

OPEN-OBJ-LIST is a list of cp--Open. Each must be a child of the
next in the list. This is used to scan-lists efficiently."
  (let ((buf-pos (and open-obj-list
                      ;; scan_lists tolerates buf-pos past EOB
                      (1+ (cp--Open-position (car open-obj-list))))))
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
      ;; Process the broader region spanned by ps-opens. There's no
      ;; need to consider other children lists lying outside the
      ;; JIT lock region.
      ;;
      ;; We mostly avoid further sexp parsing in the broader region,
      ;; except to check for multiline string just before setting
      ;; inconsistent.
      (dolist (ps-open-i ps-opens)
        (push (make-cp--Open :position
                             ps-open-i
                             :column
                             (progn
                               (goto-char ps-open-i)
                               (current-column)))
              open-objs))
      (cp--set-closes open-objs)
      (push (current-time) timing-info)
      ;; Filter out parens which don't need consideration outside the
      ;; JIT lock region. The ones that do are currently fontified as
      ;; inconsistent, and could become consistent if all its enclosed
      ;; lines are checked. The filtering of open-objs is for
      ;; performance.
      (setq open-objs
            (let* ((objs-head (cons nil open-objs))
                   (prev-open objs-head)
                   (open-i (cdr objs-head)))
              (while open-i
                (let* ((inconsistency-offset
                        (get-text-property (cp--Open-position (car open-i))
                                           'cp-inconsistency))
                       (inconsistency-pos
                        (and inconsistency-offset
                             (+ (cp--Open-position (car open-i))
                                inconsistency-offset))))
                  (if (or (not inconsistency-pos)
                          ;; Spot check using the cached offset to
                          ;; possibly avoid a complete check in
                          ;; cp--region-check-opens.
                          ;;
                          ;; Because of buffer changes,
                          ;; inconsistency-pos is not necessarily
                          ;; the original. Just do a valid check.
                          (and (< (cp--Open-position (car open-i))
                                  inconsistency-pos)
                               (<= inconsistency-pos
                                   (cp--Open-close (car open-i)))
                               (progn
                                 (goto-char inconsistency-pos)
                                 (cp--line-check-opens (list (car open-i)))
                                 (cp--Open-inconsistent (car open-i)))))
                      ;; Remove (car open-i) from list
                      (setcdr prev-open (cdr open-i))
                    (pop prev-open))
                  (pop open-i)))
              (cdr objs-head)))
      (push (current-time) timing-info)
      (when open-objs
        ;; Check lists beginning before JIT lock's region (could
        ;; scan to after JIT lock's region)
        (let ((open-objs-reversed (reverse open-objs)))
          (goto-char (cp--Open-position (car open-objs-reversed)))
          (cp--region-check-opens open-objs-reversed
                                  nil)))
      (push (current-time) timing-info)
      (goto-char start)
      ;; Process within the inputted JIT lock region
      (let* (;; Sparse vector of open paren data, indexed by position
             ;; in buffer minus start. This benchmarked better than
             ;; keeping a stack of cp--Open objects updated from the
             ;; parse states of syntax-ppss.
             (open-paren-table (make-vector (- end start) nil)))
        (while (< (point) end)
          (let ((indent-pos (progn (back-to-indentation)
                                   (point)))
                ;; Column at which text starts on the line
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
                            (- indent-pos (cp--Open-position open-obj))))))))
            ;; Go to next line. Since we already know line-end, use it
            ;; instead of rescanning the line
            (goto-char (1+ line-end))))
        (push (current-time) timing-info)
        ;; Process parens beginning in the JIT lock region but extending after
        (let ((ps-opens (nth 9 (syntax-ppss end)))
              ;; Inner to outer going towards the tail
              (open-obj-list nil))
          (dolist (ps-open-i ps-opens)
            (when (<= start ps-open-i)
              (push (or (aref open-paren-table
                              (- ps-open-i start))
                        ;; Open parens on the last line of the JIT
                        ;; lock region don't have a cp--Open object
                        ;; created yet.
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
          (cp--set-closes open-obj-list)
          (goto-char end)
          (cp--region-check-opens nil open-obj-list))
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
          ;; Apply the font color via text properties
          (with-silent-modifications
            (dolist (pos-i (list (cp--Open-position open-i)
                                 (cp--Open-close open-i)))
              (when pos-i ; TODO: handle mismatched
                (if (cp--Open-inconsistent open-i)
                    (add-text-properties pos-i
                                         (1+ pos-i)
                                         `(cp-inconsistency
                                           ,(cp--Open-inconsistent open-i)
                                           font-lock-face
                                           cp-inconsistent
                                           rear-nonsticky
                                           t))
                  (remove-text-properties pos-i
                                          (1+ pos-i)
                                          '(cp-inconsistency
                                            nil
                                            font-lock-face
                                            nil
                                            rear-nonsticky
                                            nil)))))))
        (push (current-time) timing-info)
        ;; (my-msg "cp-color-parens start=%s end=%s timing: %s"
        ;;         start end
        ;;         (my-time-diffs (nreverse timing-info)))
        ))))

(defun color-parens-unpropertize-region (start end)
  ;; TODO: remove-text-properties
  )

(defsubst color-parens-extend-region-after-change (start end _old-len)
  ;; It seems redisplay works its way from before start to after end,
  ;; so it's more important to expand the start in order to get
  ;; correct redisplays.
  (save-excursion
    (setq jit-lock-start
          (or (syntax-ppss-toplevel-pos (syntax-ppss start))
                             start))))

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
    ;; TODO: Remove from jit-lock-after-change-extend-region-functions
    (jit-lock-unregister 'cp-propertize-region)
    (color-parens-unpropertize-region (point-min) (point-max))))

(provide 'color-parens)

;;; color-parens.el ends here
