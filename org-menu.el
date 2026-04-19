;;; org-menu.el --- Compat file following rename of this library to org-cockpit

;; Copyright 2026 Jan Rehders
;;
;; Author: Jan Rehders <nospam@sheijk.net>
;; Version: 0.5.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This library has been renamed to org-cockpit. Please update all references
;; to org-menu to org-cockpit in your config. This compat file will be kept
;; for a while before it will be removed

(require 'org-cockpit)

;;;###autoload (autoload 'org-menu "org-menu" nil t)
(define-obsolete-function-alias org-menu #'org-cockpit
  "2026-04-19"
  "org-menu has been renamed to org-cockpit")

(provide 'org-menu)
;;; org-menu.el ends here
