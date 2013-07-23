;;; lisp-tests.el --- Test suite for lisp.el

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)

(defmacro lisp-tests-with-temp-buffer (contents &rest body)
  "Create a `lisp-mode' enabled temp buffer with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun lisp-tests-look-at (search-string)
  (re-search-forward string)
  (forward-char (- (length (match-string-no-properties 0)))))

(ert-deftest lisp-test-indent-line ()
  (lisp-tests-with-temp-buffer
   "
  (defun func ()
    (let ((x 10))
          ;; Next line at wrong indentation
    (y (some-func 20))))
"
   (lisp-tests-look-at "(y ")
   (indent-for-tab-command)
   (should (string= (buffer-string)
                    "
  (defun func ()
    (let ((x 10))
          ;; Next line at wrong indentation
          (y (some-func 20))))
"))))

;; Note: When | point, what to do when DEL?
(defun func ()
  (let ((x 10) (y (some-func 20)))
    (message (format "Inside func")
             |(format "foo") "bar"
             "baz")))
;; Proposition: For simplicity, have rule that if multi-line close paren not on same line as point, do ordinary DEL

;; TODO: Account for this case when | point, then DEL
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               "abc"
               |(format "foo") "bar") (progn))) ; NB: (progn) is outside of message, but formatting suggests otherwise
;; Possibility 1: violates invariant, see NB
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               "abc")
      |(format "foo" "bar") (progn))) ; NB: Violates principle of only deleting close parens whose opening is not on the same line
;; Possibility 2: allows TAB to be an inverse
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               "abc")
      |(format "foo") "bar") (progn)) ; NB: (progn) is outside of let, but formatting suggests otherwise
;; Possibility 3: TAB is not an inverse
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               "abc")
      |(format "foo") "bar" (progn))) ; NB: Take the close paren that matches to the first open paren of the previous line
;; TODO: Possibility 2 and TAB
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               "abc"
               |(format "foo") "bar") (progn))) ; NB: Back to previous state
;; TODO: Possibility 3 and TAB
  (defun func ()
    (let ((x 10) (y (some-func 20)))
      (message (format "Inside func")
               "abc"
               |(format "foo") "bar" (progn)))) ; NB: Now (progn) is inside message form when it wasn't before
                                                ; (The price paid for badly placed (progn))?

;; Note: When | point
(if cond
    (progn (expr-1
            |) (expr-2))
  else)
;; Then DEL :
(if cond
    (progn (expr-1)
           (expr-2))
  else)

;; Note: When | point, what to do when DEL?
(if cond
    (progn (expr-1
    |
            ) (expr-2))
  else)
;; Possibility 1:
(if cond
    (progn (expr-1)
           |(expr-2))
  else)
;; Possibility 2: Do as DEL would otherwise do
(if cond
    (progn (expr-1
   |
            ) (expr-2))
  else)

;; If TAB, what to do?
(if cond
    (progn (lambda () x)
           |foo) bar
  y)
;; We know we want foo to indent to the right. Whether the close
;; paren is after 'foo)' or after 'bar' is of no significance.

