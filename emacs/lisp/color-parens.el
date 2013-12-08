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
(cl-defstruct color-parens--Open position column inconsistent)

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
        (push (make-color-parens--Open :position open-pos
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
                          (or (color-parens--Open-inconsistent (car open-i))
                              (<= text-column
                                  (color-parens--Open-column (car open-i)))))
                (my-msg "DEBUG: Marking inconsistent: %s point=%s text-column=%s paren-stack=%s" (car open-i) (point) text-column paren-stack) 
                (setf (color-parens--Open-inconsistent (car open-i))
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
                                                    (color-parens--Open-position (car paren-stack))))
                                      (pop cons-i))
                                    cons-i))))
            ;; Process parens that closed upon going to this next line
            (my-msg "DEBUG: paren-stack before updating: %s" paren-stack) 
            (while (and paren-stack
                        (or (not common-open)
                            (/= (color-parens--Open-position (car paren-stack))
                                (car common-open))))
              (let ((close-pos (condition-case nil
                                   (1- (scan-lists (color-parens--Open-position (car paren-stack))
                                                   1 0))
                                 (scan-error nil)))
                    (open-obj (pop paren-stack)))
                ;; TODO: Set mismatched paren face if close-pos is nil
                ;; TODO: If operating on a subset region, it's not
                ;; always correct to color consistent
                (color-parens--update-inconsistency-colors
                 (color-parens--Open-inconsistent open-obj)
                 (color-parens--Open-position open-obj)
                 close-pos)))
            (my-msg "DEBUG: paren-stack after pops: %s" paren-stack) 
            ;; Create new color-parens--Open objects
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
                ;; here, or add color-parens--Open objects on lines
                ;; that begin in a string (leaning towards former).
                (push (make-color-parens--Open :position open-i
                                               :column (current-column))
                      paren-stack))))))
      (dolist (open-i paren-stack)
        (color-parens--update-inconsistency-colors
         (color-parens--Open-inconsistent open-i)
         (color-parens--Open-position open-i)
         (condition-case nil
             (1- (scan-lists (color-parens--Open-position open-i)
                             1 0))
           (scan-error nil)))))))

(defun cp-propertize-region-2 (start end)
  (save-excursion
    (goto-char start)
    (let* ((timing-info (list (current-time)))
           (table-start (or (car (nth 9 (syntax-ppss)))
                            start))
           ;; Sparse vector of open paren data, indexed by position in
           ;; buffer minus table-start. The purpose is speed through
           ;; non redundant calculation of current-column.
           (open-paren-table (make-vector (- end table-start) nil))
           ;; List of all color-parens--Open objects created
           (open-objs nil))
      (push (current-time) timing-info)
      (while (< (point) end)
        (let (;; Column at which text starts on the line, except if
              ;; inside a string. Text doesn't start in a comment,
              ;; since ; is text.
              (text-column (progn (back-to-indentation)
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
              (let ((open-obj (or (aref open-paren-table
                                        (- open-pos table-start))
                                  (progn
                                    (push (make-color-parens--Open
                                           :position open-pos
                                           :column (save-excursion
                                                     (goto-char open-pos)
                                                     (current-column)))
                                          open-objs)
                                    (aset open-paren-table
                                          (- open-pos table-start)
                                          (car open-objs))))))
                (when (<= text-column
                          (color-parens--Open-column open-obj))
                  (setf (color-parens--Open-inconsistent open-obj)
                        t)))))
          ;; Go to next line. Since we already know line-end, use it
          ;; instead of rescanning the line
          (goto-char (min (1+ line-end) (point-max)))))
      (push (current-time) timing-info)
      (dolist (open-i open-objs)
        ;; TODO: It might be possible to speed close-pos
        ;; calculation by setting "last known position and depth
        ;; inside list" at each line for each color-parens--Open
        ;; object. Then here use scan-lists from that info. The
        ;; performance gain is not certain, so would need to be
        ;; measured. (Note however that the iteration over lines
        ;; above is measured to be the significant time
        ;; consumer, not the iteration over open-objs here.)
        (let ((close-pos (condition-case nil
                             (1- (scan-lists (color-parens--Open-position open-i) 1 0))
                           (scan-error nil))))
          (if (color-parens--Open-inconsistent open-i)
              (color-parens--colorize (list (color-parens--Open-position open-i)
                                            close-pos)
                                      'color-parens-inconsistent)
            ;; If open paren is consistent, we've only proved
            ;; it's ok to clear the inconsistency color if open
            ;; and close were in the region.
            (when (and (<= start (color-parens--Open-position open-i))
                       (< close-pos end))
              (color-parens--decolorize (list (color-parens--Open-position open-i)
                                              close-pos))))))
      (push (current-time) timing-info)
      (my-msg "DEBUG: cp-color-parens timing: %s"
              (my-time-diffs (nreverse timing-info)))
      )))

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
                            (or (color-parens--Open-inconsistent (car open-i))
                                (<= text-column
                                    (or (color-parens--Open-column (car open-i))
                                        ;; Lazy computation of column
                                        (save-excursion
                                          (goto-char (color-parens--Open-position (car open-i)))
                                          (setf (color-parens--Open-column (car open-i)) (current-column)))))))
                  (setf (color-parens--Open-inconsistent (car open-i))
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
                  ;; Note: color-parens--Open's column field is
                  ;; initialized nil and computed lazily. This avoids
                  ;; computing current-column for open parens closed
                  ;; on the same line. These also tend to be further
                  ;; from the beginning of line.
                  (push (make-color-parens--Open :position (1- (point)))
                        paren-stack))
                 ;; Case: stopped at close paren
                 ((< 0 depth-change)
                  (if paren-stack
                      (progn
                        (color-parens--update-inconsistency-colors
                         (color-parens--Open-inconsistent (car paren-stack))
                         (color-parens--Open-position (car paren-stack))
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

(defalias 'cp-propertize-region 'cp-propertize-region-3)

(define-minor-mode color-parens-mode
  "Color unbalanced parentheses and parentheses inconsistent with
  indentation."
  nil nil nil
  (if color-parens-mode
      (progn
        (jit-lock-register (lambda (start end)
                             (apply 'cp-propertize-region
                                    (color-parens-extend-region start end)))
                           t)
        (add-hook 'jit-lock-after-change-extend-region-functions
                  'color-parens-extend-region-after-change
                  nil
                  t))
    (jit-lock-unregister 'cp-propertize-region)
    (color-parens-unpropertize-region (point-min) (point-max))))

(provide 'color-parens)

;;; color-parens.el ends here
