;;; org-menu-tests.el --- Tests for org-menu
;;
;; Copyright 2021 Jan Rehders
;;
;; Author: Jan Rehders <nospam@sheijk.net>
;; Version: 0.0
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; Tests for org-menu. Load this file, run `(ert)' and search for org-menu

(require 'org-menu)
(require 'ert)

(defun org-menu-test-with-fn (left-text right-text function)
  (with-temp-buffer
    (org-mode)
    (insert left-text)
    (let ((center (point)))
      (insert right-text)
      (goto-char center)
      (funcall function))))

(defmacro org-menu-test-with (left-text right-text &rest code)
  (declare (indent defun))
  `(org-menu-test-with-fn ,left-text ,right-text (lambda () ,@code)))

(defun org-menu-test-formatting (format-char left-text right-text)
  (org-menu-test-with left-text right-text
    (let ((range (org-menu-parse-formatting format-char)))
      (buffer-substring (car range) (cdr range)))))

(ert-deftest org-menu-test-parse-formatting ()
  (should (string-equal "*foobar*"
                        (org-menu-test-formatting ?* "*foo" "bar*")))
  (should (string-equal "*/foobar/*"
                        (org-menu-test-formatting ?* "*/foo" "bar/*")))
  (should (string-equal "*/foobar/*"
                        (org-menu-test-formatting ?* "=*/foo" "bar/*=")))
  (should (string-equal "/foobar/"
                        (org-menu-test-formatting ?/ "=*/foo" "bar/*=")))
  (should (null (org-menu-test-with "foo" "bar"
                  (org-menu-parse-formatting ?=)))))

(provide 'org-menu-test)
;;; org-menu-tests.el ends here
