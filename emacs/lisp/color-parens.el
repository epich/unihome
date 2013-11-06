;;; color-parens.el --- Color unbalanced parentheses and parentheses inconsistent with indentation -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Barry O'Reilly <gundaetiapo@gmail.com>
;; Version: 1.2

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

(defface color-parens-inconsistent-open
  ;; TODO: Fix colors: something orange to red-orange, no background
  '((((class color) (background light))
     :background "turquoise")		; looks OK on tty (becomes cyan)
    (((class color) (background dark))
     :background "steelblue3")		; looks OK on tty (becomes blue)
    (((background dark))
     :background "grey50")
    (t
     :background "gray"))
  "Face to use for an open paren whose close paren is
inconsistent with the indentation within."
  :group 'color-parens-faces)

(defface color-parens-inconsistent-close
  '((t :inherit color-parens-inconsistent-open))
  "Face to use for a close paren inconsistent with the
indentation within it."
  :group 'color-parens-faces)

;; TODO: Faces for mismatched open and close

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

(defun color-parens-propertize-region (start end)
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    ;; To start each iteration of a line, start char at end of previous line
    ;;
    ;; This is one way to facilitate the edge case of an end of line
    ;; being EOB.
    (unless (bobp) (forward-char -1))
    (let (;; Push at open parens, pop at close parens
          (paren-stack)
          (parse-state '(0 nil nil nil nil nil nil nil nil)))
      (while (< (point) end)
        ;; Advance from end of line to beginning of next line
        (forward-char 1)
        (let ((line-start (point))
              ;; Column at which text starts on the line
              (text-column (progn (back-to-indentation)
                                  (current-column)))
              (line-end (progn (end-of-line)
                               (point))))
          ;; Mark open parens on the paren-stack that become
          ;; inconsistent because of the current line.
          ;;
          ;; Loop invariant: All close-parens--Open which are marked
          ;; inconsistent are contiguous on the stack to the bottom.
          ;; This follows from the fact that marking one inconsistent
          ;; causes all others below it to become inconsistent too.
          (unless (and paren-stack
                       (< (color-parens--Open-column (car paren-stack))
                          text-column))
            (let ((open-i paren-stack))
              (while (and open-i
                          ;; Because of the loop invariant, only go
                          ;; down the stack until the first not
                          ;; inconsistent
                          (not (color-parens--Open-inconsistent (car open-i))))
                (setf (color-parens--Open-inconsistent (car open-i))
                      t)
                (setq open-i (cdr open-i)))))
          (goto-char line-start)
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
              (cond ((= 0 depth-change)
                     ;; Keep parsing
                     nil)
                    ;; Case: stopped at open paren
                    ((< depth-change 0)
                     ;; Push
                     (setq paren-stack
                           (cons (make-color-parens--Open :position (1- (point))
                                                          :column text-column)
                                 paren-stack)))
                    ;; Case: stopped at close paren
                    ((< 0 depth-change)
                     (when (color-parens--Open-inconsistent (car paren-stack))
                       (with-silent-modifications
                         ;; Change font lock color: close paren
                         (add-text-properties (1- (point))
                                              (point)
                                              '(font-lock-face
                                                color-parens-inconsistent-close
                                                rear-nonsticky
                                                t))
                         ;; Change font lock color: open paren
                         (add-text-properties (color-parens--Open-position (car paren-stack))
                                              (1+ (color-parens--Open-position (car paren-stack)))
                                              '(font-lock-face
                                                color-parens-inconsistent-close
                                                rear-nonsticky
                                                t))))
                     ;; Pop
                     ;; TODO: Handle case of popping nil paren-stack
                     (setq paren-stack
                           (cdr paren-stack)))))))))))

(defun color-parens-unpropertize-region (start end)
  ;; TODO: remove-text-properties
  )

(define-minor-mode color-parens-mode
  "Color unbalanced parentheses and parentheses inconsistent with
  indentation."
  nil nil nil
  (if color-parens-mode
      (jit-lock-register 'color-parens-propertize-region t)
    (jit-lock-unregister 'color-parens-propertize-region)
    (color-parens-unpropertize-region (point-min) (point-max))))

(provide 'color-parens)

;;; color-parens.el ends here
