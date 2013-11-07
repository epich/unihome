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

;; TODO: Algorithm doesn't account for:
;;
;; (abc
;;   (def))
;;  (ghi)
;;
;; (abc ...) are inconsistent parens because (ghi) is indented too far

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
          ;; Skip whitespace only lines
          (unless (eq text-column line-end)
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
                (setq open-i (cdr open-i))))
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
                                   paren-stack))
                       (message "Pushed %s" (car paren-stack)))
                      ;; Case: stopped at close paren
                      ((and (< 0 depth-change)
                            paren-stack)
                       (if (color-parens--Open-inconsistent (car paren-stack))
                           ;; Parens inconsistent, change font lock
                           ;; for close and open paren
                           (color-parens--colorize
                            (list (1- (point))
                                  (color-parens--Open-position (car paren-stack)))
                            'color-parens-inconsistent)
                         ;; Parens consistent, restore normal font
                         ;; lock to close and open paren
                         (color-parens--decolorize
                          (list (1- (point))
                                (color-parens--Open-position (car paren-stack)))))
                       ;; Pop
                       ;; TODO: Handle case of popping nil paren-stack
                       (message "Pushing %s" (car paren-stack))
                       (setq paren-stack
                             (cdr paren-stack))))))))))))

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
