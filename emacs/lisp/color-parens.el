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

;;; Code:

(require 'cl-lib)

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

;; TODO: Faces for mismatched open and close

;; TODO: Remove debugging message statements

;; TODO: Test close parens in doc of c-beginning-of-statement-1 in
;; cc-engine.el

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

POSITIONS is a list of positions in the buffer to colorize."
  (with-silent-modifications
    (mapc (lambda (pos-i)
            (add-text-properties pos-i
                                 (1+ pos-i)
                                 `(font-lock-face
                                   ,face-arg
                                   rear-nonsticky
                                   t)))
          positions)))

(defsubst color-parens--decolorize (positions)
  "Decolorize chars in the buffer colored with Font Lock.

POSITIONS is a list of positions in the buffer to colorize."
  (with-silent-modifications
    (mapc (lambda (pos-i)
            (remove-text-properties pos-i
                                    (1+ pos-i)
                                    '(font-lock-face
                                      nil
                                      rear-nonsticky
                                      nil)))
          positions)))

(defsubst color-parens--update-inconsistency-colors (inconsistentp
                                                     open-paren
                                                     close-paren)
  "Update inconsistency Font Lock colors for OPEN-PAREN and
CLOSE-PAREN as buffer positions based on INCONSISTENTP."
  (if inconsistentp
      (color-parens--colorize (list open-paren close-paren)
                              'color-parens-inconsistent)
    (color-parens--decolorize (list open-paren close-paren))))

(defun color-parens-propertize-region (start end)
  (message "Starting start=%s end=%s" start end)
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (let (;; Push at open parens, pop at close parens
          (paren-stack)
          (parse-state (syntax-ppss)))
      (while (< (point) end)
        (let ((line-start (point))
              ;; Column at which text starts on the line, except if
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
                ;; the first inconsistent=nil Open with small enough
                ;; column.
                (while (and open-i
                            (or (<= text-column
                                    (color-parens--Open-column (car open-i)))
                                (color-parens--Open-inconsistent (car open-i))))
                  (setf (color-parens--Open-inconsistent (car open-i))
                        t)
                  (setq open-i (cdr open-i)))))
            ;; Note: point is at indentation
            (while (and (< (point) line-end))
              (let ((depth-change
                     (- (car parse-state)
                        (car (setq parse-state
                                   ;; TODO: Will it perform better not
                                   ;; parsing 1 char at a time?
                                   (parse-partial-sexp (point)
                                                       (1+ (point))
                                                       nil
                                                       nil
                                                       parse-state))))))
                (cond
                 ((or (= 0 depth-change)   ; Didn't cross a paren
                      (nth 3 parse-state)  ; Inside a string
                      (nth 4 parse-state)) ; Inside a comment
                  nil) ; Keep parsing
                 ;; Case: stopped at open paren
                 ((< depth-change 0)
                  ;; Push
                  (setq paren-stack
                        (cons (make-color-parens--Open :position (1- (point))
                                                       :column text-column)
                              paren-stack))
                  (message "Pushed: %s" (car paren-stack)))
                 ;; Case: stopped at close paren
                 ((< 0 depth-change)
                  (if paren-stack
                      (progn
                        (color-parens--update-inconsistency-colors
                         (color-parens--Open-inconsistent (car paren-stack))
                         (color-parens--Open-position (car paren-stack))
                         (1- (point)))
                        ;; Pop
                        (message "Popping: %s" (car paren-stack))
                        (setq paren-stack
                              (cdr paren-stack)))
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

(defun color-parens-extend-region ()
  "Extend region for JIT lock to fontify."
  (message "DEBUG: Start color-parens-extend-region font-lock-beg=%s font-lock-end=%s" font-lock-beg font-lock-end) 
  (save-excursion
    (let ((top-level (syntax-ppss-toplevel-pos (syntax-ppss font-lock-beg))))
      (when top-level
        (setq font-lock-beg (min font-lock-beg top-level))
        (goto-char top-level)
        (setq font-lock-end (max font-lock-end
                                (or (scan-lists (point) 1 0)
                                    (point-max)))))))
  (message "color-parens-extend-region font-lock-beg=%s font-lock-end=%s" font-lock-beg font-lock-end))

(define-minor-mode color-parens-mode
  "Color unbalanced parentheses and parentheses inconsistent with
  indentation."
  nil nil nil
  (if color-parens-mode
      (progn
        (jit-lock-register 'color-parens-propertize-region t)
        (add-hook 'font-lock-extend-region-functions
                  'color-parens-extend-region
                  nil
                  t))
    (jit-lock-unregister 'color-parens-propertize-region)
    (color-parens-unpropertize-region (point-min) (point-max))))

(provide 'color-parens)

;;; color-parens.el ends here
