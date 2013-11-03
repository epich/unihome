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
    (unless (bob) (forward-char -1))
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
          ;; inconsistent are contiugous on the stack. This follows
          ;; from the fact that marking one inconsistent causes all
          ;; others below it to become inconsistent too.
          (unless (and paren-stack
                       (< (color-parens--Open-column paren-stack)
                          text-column))
            (let ((paren-stack-i paren-stack))
              (while (and paren-stack-i
                          (not (color-parens--Open-inconsistent paren-stack-i)))
                (setf (color-parens--Open-inconsistent paren-stack-i)
                      t)
                (setq paren-stack-i (cdr paren-stack-i)))))
          (goto-char line-start)
          (while (and (< (point) line-end))
            (let ((depth-change
                   (- (car parse-state)
                      (car (setq parse-state
                                 (parse-partial-sexp (point)
                                                     (1+ (point))
                                                     nil
                                                     nil
                                                     parse-state))))))
              (cond ((= 0 depth-change)
                     ;; Keep parsing
                     nil)
                    ;; Stopped at open paren
                    ((< depth-change 0)
                     ;; Push
                     (setq paren-stack
                           (cons (make-color-parens--Open :position (1- (point))
                                                          :column text-column)
                                 paren-stack)))
                    ;; Stopped at close paren
                    ((< 0 depth-change)
                     ;; Change font lock color of close paren and it's open paren
                     ;; TODO: close paren: (1- (point)) and open paren: (color-parens--Open-position (car paren-stack))
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
