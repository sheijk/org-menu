;;; org-menu.el --- A discoverable menu for Emacs org-mode using transient -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright 2021 Jan Rehders
;;
;; Author: Jan Rehders <nospam@sheijk.net>
;; Version: 0.1alpha
;; Package-Requires: ((emacs "26.1") (transient "0.1"))
;; URL: https://github.com/sheijk/org-menu
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
;; Usage:
;;
;; Add this to your ~/.emacs to bind the menu to `C-c m':
;;
;; (with-eval-after-load 'org
;;   (require 'org-menu) ;; not needed if installing by package manager
;;   (define-key org-mode-map (kbd "C-c m") 'org-menu))
;;
;; The menu should be pretty self-explanatory. It is context dependent and
;; offers different commands for headlines, tables, timestamps, etc.
;; The task menu provides entry points for task that work from anywhere.
;;
;;; Code:

(require 'org)
(require 'transient)

(defun org-menu-heading-navigate-items (check-for-heading)
  "Items to navigate headings.

These will be added to most sub menus."
  `(["Navigate"
     ,@(when check-for-heading '(:if org-at-heading-p))
     ("p" "prev" org-previous-visible-heading :transient t)
     ("n" "next" org-next-visible-heading :transient t)
     ("c" "cycle" org-cycle :transient t)
     ("u" "parent" outline-up-heading :transient t)
     ("M-p" "prev (same level)" org-backward-heading-same-level :transient t)
     ("M-n" "next (same level)" org-forward-heading-same-level :transient t)]))

(defun org-menu-show-headline-content ()
  "Will show the complete content of the current headline and it's children."
  (interactive)
  (save-excursion
    (outline-hide-subtree)
    (org-show-children 4)
    (org-goto-first-child)
    (org-reveal '(4))))

(transient-define-prefix org-menu-visibility ()
  "A menu to control visibility of org-mode items"
  ["dummy"])

(transient-insert-suffix 'org-menu-visibility (list 0)
  `["Visibility"
    ,@(org-menu-heading-navigate-items nil)
    ["Visibility"
     ("a" "all" org-show-subtree :if-not org-at-block-p :transient t)
     ("a" "all" org-hide-block-toggle :if org-at-block-p :transient t)
     ("t" "content" org-menu-show-headline-content :if-not org-at-block-p :transient t)
     ("h" "hide" outline-hide-subtree :if-not org-at-block-p :transient t)
     ("h" "hide" org-hide-block-toggle :if org-at-block-p :transient t)
     ("r" "reveal" (lambda () (interactive) (org-reveal t)) :if-not org-at-block-p :transient t)]
    ["Global"
     ("C" "cycle global" org-global-cycle :transient t)
     ("go" "overview" org-overview)
     ("gc" "content" org-content)
     ("ga" "all" org-show-all)
     ("gd" "default" (lambda () (interactive) (org-set-startup-visibility)))]
    [("q" "quit" transient-quit-all)]])

(defun org-menu-eval-src-items ()
  "Return the items to evaluate a source block"
  (list
   ["Source"
    :if org-in-src-block-p
    ("e" "run block" org-babel-execute-src-block)
    ("c" "check headers" org-babel-check-src-block)
    ("k" "clear results" org-babel-remove-result-one-or-many)
    ("'" "edit" org-edit-special)]))

(transient-define-prefix org-menu-eval ()
  "A menu to evaluate buffers, tables, etc. in org-mode"
  ["dummy"])

(transient-insert-suffix 'org-menu-eval (list 0)
  `["Evaluation"
    ["Table"
     :if org-at-table-p
     ("e" "table" (lambda () (interactive) (org-table-recalculate 'iterate)))
     ("1" "one iteration" (lambda () (interactive) (org-table-recalculate t)))
     ("l" "line" (lambda () (interactive) (org-table-recalculate nil)))
     ("f" "format" org-table-align :if org-at-table-p)]
    ,@(org-menu-eval-src-items)
    ["Heading"
     :if-not org-in-src-block-p
     ("c" "update checkbox count" org-update-checkbox-count)]
    [("q" "quit" transient-quit-all)]])

(defun org-menu-insert-block (str)
  "Insert an org mode block of type `str'"
  (interactive)
  (insert (format "#+begin_%s\n#+end_%s\n" str str)))

(defun org-menu-expand-snippet (snippet)
  "Will expand the given snippet."
  (interactive)
  (insert snippet)
  (yas-expand))

(transient-define-prefix org-menu-insert-blocks ()
  "A menu to insert new blocks in org-mode"
  [["Insert block"
    ("s" "source" (lambda () (interactive) (org-menu-insert-block "src")))
    ("e" "example" (lambda () (interactive) (org-menu-insert-block "example")))
    ("v" "verbatim" (lambda () (interactive) (org-menu-insert-block "verbatim")))
    ("a" "ascii" (lambda () (interactive) (org-menu-insert-block "ascii")))
    ("q" "quote" (lambda () (interactive) (org-menu-insert-block "quote")))
    ("d" "dynamic block" org-insert-dblock)]
   [("q" "quit" transient-quit-all)]])

(transient-define-prefix org-menu-insert-heading ()
  "A menu to insert new headings in org-mode"
  [["Heading"
    ("h" "heading" org-insert-heading)
    ("H" "heading (after)" org-insert-heading-after-current)
    ("T" "todo" org-insert-todo-heading)]
   ["Items"
    ("d" "drawer" org-insert-drawer)]
   [("q" "quit" transient-quit-all)]])

(transient-define-prefix org-menu-insert-template ()
  "A menu to insert new templates in org-mode"
  [["Templates"
    ("S" "structure template" org-insert-structure-template)
    ("B" "yas blocks" (lambda () (interactive) (org-menu-expand-snippet "beg")))
    ("O" "yas options" (lambda () (interactive) (org-menu-expand-snippet "opt")))]
   [("q" "quit" transient-quit-all)]])

(transient-define-prefix org-menu-insert-timestamp ()
  "A menu to insert timestamps in org-mode"
  [["Timestamp"
    ("." "active" org-time-stamp)
    ("!" "inactive" org-time-stamp-inactive)]
   ["Now"
    ("n" "active" (lambda () (interactive) (org-insert-time-stamp (current-time) t)))
    ("N" "inactive" (lambda () (interactive) (org-insert-time-stamp (current-time) t t)))]
   ["Today"
    ("t" "active" (lambda () (interactive) (org-insert-time-stamp (current-time) nil)))
    ("T" "inactive" (lambda () (interactive) (org-insert-time-stamp (current-time) nil t)))]
   [("q" "quit" transient-quit-all)]])

(defun shk-org-menu-table-insert-row-below ()
  "Insert a new table column below point"
  (interactive)
  (org-table-insert-row '4))

(defun shk-org-menu-table-insert-column-left ()
  "Insert a new column to the left of point"
  (interactive)
  (org-table-insert-column)
  (org-table-move-column-right))

(transient-define-prefix org-menu-insert-table ()
  "A menu to insert table items in org-mode"
  [["Table"
    ("t" "table" org-table-create-or-convert-from-region :if-not org-at-table-p)]
   ["Rows/columns"
    :if org-at-table-p
    ("r" "row above" org-table-insert-row :transient t)
    ("R" "row below" shk-org-menu-table-insert-row-below :transient t)
    ("c" "column right" org-table-insert-column :transient t)
    ("C" "column left" shk-org-menu-table-insert-column-left :transient t)
    ("-" "horiz. line" org-table-insert-hline :transient t)]
   [("i" "insert other element" org-menu-insert)
    ("q" "quit" transient-quit-all)]])

(transient-define-prefix org-menu-insert ()
  "A menu to insert new items in org-mode"
  [["Insert"
    ("." "time" org-menu-insert-timestamp)
    ("t" "table" org-menu-insert-table)
    ("h" "heading" org-menu-insert-heading)
    ("b" "block" org-menu-insert-blocks)
    ("T" "templates" org-menu-insert-template)
    ("l" "link" org-insert-link)]
   [("q" "quit" transient-quit-all)]])

(defun org-menu-comment-line ()
  "Toggles line comment w/o moving cursor"
  (interactive)
  (save-excursion (comment-line 1)))

(defun org-menu-insert-text (left right)
  "Will insert left|right and put the curser at |"
  (if (region-active-p)
      (progn
        (insert left)
        (exchange-point-and-mark)
        (insert right))
    (insert left)
    (insert right)
    (backward-char (length right))))

(defun org-menu-in-time-p ()
  "Returns whether we're at a time stamp or similar

Adapted from `org-goto-calendar'"
  (or (org-at-timestamp-p 'lax)
      (org-match-line (concat ".*" org-ts-regexp))))

(transient-define-prefix org-menu-goto ()
  "Menu to go to different places by name"
  ["Go to"
   ("s" "source block" org-babel-goto-named-src-block)
   ("r" "result block" org-babel-goto-named-result)
   ("h" "heading" imenu)
   ("." "calendar" org-goto-calendar :if org-menu-in-time-p)]
  [("q" "quit" transient-quit-all)])

(defun org-menu-at-text-p ()
  "Returns whether point is at text"
  (not (or (org-at-heading-p)
           (org-at-table-p)
           (org-in-item-p)
           (org-in-src-block-p))))

(defun org-menu-text-format-items (check-for-table)
  (list
   `["Navigate"
     ,@(when check-for-table '(:if org-menu-at-text-p))
     ("p" "up" previous-line :transient t)
     ("n" "down" next-line :transient t)
     ("b" "left" backward-word :transient t)
     ("f" "right" forward-word :transient t)
     ("u" "parent" org-up-element :transient t)
     ("SPC" "mark" set-mark-command :transient t)
     ("C-x C-x" "exchange" exchange-point-and-mark :transient t)]
   `["Formatting"
     ,@(when check-for-table '(:if org-menu-at-text-p))
     ("*" "Bold" (lambda nil (interactive) (org-menu-insert-text "*" "*")) :transient t)
     ("/" "italic" (lambda nil (interactive) (org-menu-insert-text "/" "/")) :transient t)
     ("_" "underline" (lambda nil (interactive) (org-menu-insert-text "_" "_")) :transient t)
     ("+" "strikethrough" (lambda nil (interactive) (org-menu-insert-text "+" "+")) :transient t)]
   `["Source"
     ,@(when check-for-table '(:if org-menu-at-text-p))
     ("~" "code" (lambda nil (interactive) (org-menu-insert-text "~" "~")) :transient t)
     ("=" "verbatim" (lambda nil (interactive) (org-menu-insert-text "=" "=")) :transient t)]))

(transient-define-prefix org-menu-text-in-element ()
  "Add formatting for text inside other elements like lists and tables"
  ["dummy"])

(transient-insert-suffix 'org-menu-text-in-element (list 0)
  `[,@(org-menu-text-format-items nil)
    [("q" "quit" transient-quit-all)]])

(transient-define-prefix org-menu-options ()
  "A menu to toggle options"
  ["Display"
   ("l" "show links" org-toggle-link-display)
   ("i" "inline images" org-toggle-inline-images)
   ("p" "pretty entities" org-toggle-pretty-entities)
   ("t" "timestamp overlay" org-toggle-time-stamp-overlays)])

(defun org-menu-in-link ()
  "Returns whether we are inside a link.

Conditions have been adapted from `org-insert-link'"
  (or
   (org-in-regexp org-link-bracket-re 1)
   (or (org-in-regexp org-link-angle-re)
       (org-in-regexp org-link-plain-re))))

(defun org-menu-toggle-has-checkbox ()
  "Toggle whether the current list item has a checkbox"
  (interactive)
  (save-excursion
    (back-to-indentation)
    (if (not (looking-at "- "))
        (message "Not at list item")
      (end-of-line 1)
      (org-ctrl-c-ctrl-c '(4)))))

(transient-define-prefix org-menu-clock ()
  "Time management using org-modes clock"
  ["Clock"
   ("<tab>" "in" org-clock-in :if-not org-clock-is-active)
   ("TAB" "in" org-clock-in :if-not org-clock-is-active)
   ("o" "out" org-clock-out :if org-clock-is-active)
   ("j" "goto" org-clock-goto :if org-clock-is-active)
   ("Q" "cancel" org-clock-cancel :if org-clock-is-active)
   ("d" "display" org-clock-display :if org-clock-is-active)
   ("x" "in again" org-clock-in-last :if-not org-clock-is-active)
   ("z" "resolve" org-resolve-clocks)])

;;;###autoload
(transient-define-prefix org-menu ()
  "A discoverable menu to edit and view org-mode documents"
  ["dummy"])

(transient-insert-suffix 'org-menu (list 0)
  `["Org mode"
    ;; Items for headings
    ,@(org-menu-heading-navigate-items t)

    ["Move heading"
     :if org-at-heading-p
     ("P" "up" org-metaup :transient t)
     ("N" "down" org-metadown :transient t)
     ("B" "left" org-shiftmetaleft :transient t)
     ("F" "right" org-shiftmetaright :transient t)
     ("b" "left (line)" org-metaleft :transient t)
     ("f" "right (line)" org-metaright :transient t)
     ("r" "refile" org-refile :transient t)]
    ["Change heading"
     :if org-at-heading-p
     ("*" "toggle" org-ctrl-c-star :if-not org-at-table-p :transient t)
     ("t" "todo" org-todo :transient t)
     ("T" "tags" org-set-tags-command :transient t)
     ("P" "property" org-set-property :transient t)
     ("A" "archive" org-toggle-archive-tag :transient t)
     ("/" "comment" org-toggle-comment :transient t)
     ("C-w" "cut tree" org-cut-special :transient t)
     ("C-y" "yank tree" org-paste-special :transient t)]
    ["Make new/delete"
     :if org-at-heading-p
     ("mh" "make heading (before)" org-insert-heading :transient t)
     ("mH" "make heading (after)" org-insert-heading-after-current :transient t)
     ("mT" "make todo (before)" org-insert-todo-heading :transient t)
     ("dh" "delete heading" org-cut-subtree :transient t)
     ("dP" "delete property" org-delete-property :transient t)]

    ;; Items for tables
    ["Navigate"
     :if org-at-table-p
     ("p" "up" previous-line :transient t)
     ("n" "down" next-line :transient t)
     ("b" "left" org-table-previous-field :transient t)
     ("f" "right" org-table-next-field :transient t)
     ("u" "parent" outline-up-heading :transient t)]
    ["Move r/c"
     :if org-at-table-p
     ("P" "up" org-table-move-row-up :transient t)
     ("N" "down" org-table-move-row-down :transient t)
     ("B" "left" org-table-move-column-left :transient t)
     ("F" "right" org-table-move-column-right :transient t)]
    ["Field"
     :if org-at-table-p
     ("'" "edit" org-table-edit-field)
     ("SPC" "blank" org-table-blank-field :transient t)
     ("RET" "from above" org-table-copy-down :transient t)]
    ["Formulas"
     :if org-at-table-p
     ("E" "edit all" org-table-edit-formulas :transient t)
     ("=" "field" (lambda () (interactive) (org-table-eval-formula '(4))) :transient t)
     ("+" "in place" (lambda () (interactive) (org-table-eval-formula '(16))))
     ("c" "column" org-table-eval-formula :transient t)
     ("h" "coordinates" org-table-toggle-coordinate-overlays :transient t)
     ("D" "debug" org-table-toggle-formula-debugger :transient t)]
    ["Table"
     :if org-at-table-p
     ("dr" "delete row" org-shiftmetaup :transient t)
     ("dc" "delete column" org-shiftmetaleft :transient t)
     ("mr" "row above" org-table-insert-row :transient t)
     ("mc" "column right" org-table-insert-column :transient t)
     ("m-" "horiz. line" org-table-insert-hline :transient t)
     ("S" "shrink column" org-table-toggle-column-width :transient t)
     ("t" "text formatting" org-menu-text-in-element)]

    ;; Items for lists
    ["Navigate"
     :if org-in-item-p
     ("p" "prev" previous-line :transient t)
     ("n" "next" next-line :transient t)
     ("c" "cycle" org-cycle :transient t)
     ("u" "parent" org-up-element :transient t)
     ("M-p" "prev (same level)" org-backward-element :transient t)
     ("M-n" "next (same level)" org-forward-element :transient t)]
    ["Move list"
     :if org-in-item-p
     ("P" "up" org-metaup :transient t)
     ("N" "down" org-metadown :transient t)
     ("B" "left" org-shiftmetaleft :transient t)
     ("F" "right" org-shiftmetaright :transient t)
     ("b" "left (line)" org-metaleft :transient t)
     ("f" "right (line)" org-metaright :transient t)]
    ["List"
     :if org-in-item-p
     ("R" "repair" org-list-repair)
     ("*" "turn into tree" org-list-make-subtree)
     ("t" "text formatting" org-menu-text-in-element)]
    ["Toggle"
     :if org-in-item-p
     ("-" "list item" org-toggle-item :if-not org-at-table-p :transient t)
     ("+" "list style" org-cycle-list-bullet :if-not org-at-table-p :transient t)
     ("d" "done" org-toggle-checkbox :transient t)
     ("m" "checkbox" org-menu-toggle-has-checkbox :transient t)]

    ;; Items for text
    ,@(org-menu-text-format-items t)
    ["Line"
     :if org-menu-at-text-p
     (":" "fixed width" org-toggle-fixed-width :transient t)
     (";" "comment" org-menu-comment-line :transient t)
     ("--" "list" org-toggle-item :transient t)
     ("-*" "heading" org-ctrl-c-star :transient t)]

    ;; Items for source blocks
    ,@(org-menu-eval-src-items)

    ["Link"
     :if org-menu-in-link
     ("e" "edit" org-insert-link)]

    ["Timestamp"
     :if org-menu-in-time-p
     ("." "type" org-toggle-timestamp-type)
     ("e" "edit" org-time-stamp)]

    ["Tasks"
     ("v" "visibility" org-menu-visibility)
     ("x" "evaluation" org-menu-eval)
     ("i" "insert" org-menu-insert-table :if org-at-table-p)
     ("i" "insert" org-menu-insert :if-not org-at-table-p)
     ("g" "go to" org-menu-goto)
     ("o" "options" org-menu-options)
     ("C" "clock (active)" org-menu-clock :if org-clock-is-active)
     ("C" "clock" org-menu-clock :if-not org-clock-is-active)
     ""
     ("q" "quit" transient-quit-all)]])

(provide 'org-menu)
;;; org-menu.el ends here
