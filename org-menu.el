;;; org-menu.el --- A discoverable menu for org-mode using transient -*- lexical-binding: t -*-
;;
;; Copyright 2021  Jan Rehders
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

;;;; Usage:

;; Add this to your ~/.emacs to bind the menu to `C-c m':
;;
;; (with-eval-after-load 'org
;;   (require 'org-menu) ;; not needed if installing by package manager
;;   (define-key org-mode-map (kbd "C-c m") #'org-menu))
;;
;; The menu should be pretty self-explanatory.  It is context dependent and
;; offers different commands for headlines, tables, timestamps, etc.
;; The task menu provides entry points for task that work from anywhere.

;;; Code:

(require 'org)
(require 'transient)
(require 'org-capture)
(require 'org-timer)
(require 'cl-lib)

(defgroup org-menu nil
  "Options for `org-menu'."
  :group 'org)

(defcustom org-menu-use-q-for-quit t
  "Whether to add a q binding to quit to all menus.

Use this if you prefer to be consistent with magit.  It will also
change some other bindings to use Q instead of q."
  :type 'boolean)

(defcustom org-menu-global-toc-depth 10
  "The number of heading levels to show when displaying the global content."
  :type 'natnum)

(defcustom org-menu-expand-snippet-function #'org-menu-expand-snippet-default
  "The function used to expand a snippet.

See `org-menu-expand-snippet-default' for a list of snippet ids
which need to be supported.  `org-menu-expand-snippet-yasnippet'
shows how to invoke snippets."
  :type 'function)

(defun org-menu-show-columns-view-options-p ()
  "Return whether `org-columns' mode is active."
  (bound-and-true-p org-columns-overlays))

(defun org-menu-show-heading-options-p ()
  "Whether to show commands operating on headings."
  (unless (org-menu-show-columns-view-options-p)
    (org-at-heading-p)))

(defun org-menu-show-table-options-p ()
  "Whether to show commands operating on tables."
  (unless (org-menu-show-columns-view-options-p)
    (org-at-table-p)))

(defun org-menu-show-list-options-p ()
  "Whether to show commands operating on lists."
  (unless (org-menu-show-columns-view-options-p)
    (org-at-item-p)))

(defun org-menu-show-text-options-p ()
  "Whether to show commands operating on text."
  (not (or (org-menu-show-columns-view-options-p)
           (org-at-heading-p)
           (org-at-table-p)
           (org-in-item-p)
           (org-in-src-block-p))))

(defun org-menu-show-src-options-p ()
  "Whether to show commands operating on src blocks."
  (unless (org-menu-show-columns-view-options-p)
    (org-in-src-block-p)))

(defun org-menu-show-link-options-p ()
  "Whether to show commands operating on links.

Conditions have been adapted from `org-insert-link'"
  (unless (org-menu-show-columns-view-options-p)
    (or
     ;; Use variable from org-compat to support Emacs 26
     (org-in-regexp (symbol-value 'org-bracket-link-regexp) 1)
     (when (boundp 'org-link-angle-re)
       (org-in-regexp org-link-angle-re))
     (when (boundp 'org-link-plain-re)
       (org-in-regexp org-link-plain-re)))))

(defun org-menu-show-timestamp-options-p ()
  "Whether to show commands operating on timestamps."
  (unless (org-menu-show-columns-view-options-p)
    (org-at-timestamp-p 'lax)))

(defun org-menu-show-footnote-options-p ()
  "Whether to show commands operating on footnotes."
  (unless (org-menu-show-columns-view-options-p)
    (or (org-footnote-at-definition-p)
        (org-footnote-at-reference-p))))

(defun org-menu-heading-navigate-items (check-for-heading &optional cycle-function)
  "Items to navigate headings.

These will be added to most sub menus.  If `CHECK-FOR-HEADING' is
true the items will only be added if on a heading.  `CYCLE-FUNCTION' is the
function to be used to cycle visibility of current element."
  (setq cycle-function (or cycle-function #'org-cycle))
  `(["Navigate"
     :pad-keys t
     ,@(and check-for-heading '(:if org-menu-show-heading-options-p))
     ("p" "prev" org-previous-visible-heading :transient t)
     ("n" "next" org-next-visible-heading :transient t)
     ("c" "cycle" ,cycle-function :transient t)
     ("u" "parent" outline-up-heading :transient t)
     ("M-p" "prev (same level)" org-backward-heading-same-level :transient t)
     ("M-n" "next (same level)" org-forward-heading-same-level :transient t)
     ("M-w" "store link" org-store-link :transient t :if-not region-active-p)
     ("C-_" "undo" undo :transient t)]))

(defun org-menu-expand-snippet-default (snippet-id)
  "Insert a fixed text for each `SNIPPET-ID'."
  (pcase snippet-id
    ('block (insert "#+BEGIN:\n#+END:\n"))
    ('option (insert "#+"))
    ('subscript (insert "a_b"))
    ('superscript (insert "a^b"))
    ('plot
     (insert
      "#+plot: type:2d file:\"plot.svg\"
| A |  B |
|---+----|
| 1 | 10 |
| 2 |  8 |
| 3 |  9 |

#+attr_org: :width 400px
[[file:plot.svg]]
"))
    (_ (error "Unknown snippet type %s" snippet-id))))

(autoload 'yas-expand-snippet "yasnippet")
(autoload 'yas-expand-from-trigger-key "yasnippet")

(defun org-menu-expand-snippet-yasnippet (snippet-id)
  "Expand a yasnippet for each `SNIPPET-ID'."
  (unless (require 'yasnippet nil 'noerror)
    (error "Yasnippet not installed, could not expand %s" snippet-id))
  (pcase snippet-id
    ('block
     (insert "beg")
     (yas-expand-from-trigger-key))
    ('option
     (insert "opt")
     (yas-expand-from-trigger-key))
    ('subscript
     (yas-expand-snippet "${1:text}_{${2:sub}}"))
    ('superscript
     (yas-expand-snippet "${1:text}^{${2:super}}"))
    ('plot
     (yas-expand-snippet
      "#+plot: type:${1:2d} file:\"${2:plot.svg}\"
| A |  B |
|---+----|
| 1 | 10 |
| 2 |  8 |
| 3 |  9 |

#+attr_org: :width ${3:400px}
[[file:$2]]
"))
    (_
     (error "Unknown snippet type %s" snippet-id))))

;; If yasnippet gets loaded it will be used automatically
(with-eval-after-load 'yasnippet
  (unless (equal org-menu-expand-snippet-function #'org-menu-expand-snippet-default)
    (setq org-menu-expand-snippet-function #'org-menu-expand-snippet-yasnippet)))

(defun org-menu-expand-snippet (snippet-id)
  "Will expand the given snippet named `SNIPPET-ID' with `ARGS'."
  (funcall org-menu-expand-snippet-function snippet-id))

(defun org-menu-show-headline-content ()
  "Will show the complete content of the current headline and it's children."
  (interactive)
  (save-excursion
    (outline-hide-subtree)
    (org-fold-show-children 4)
    (org-goto-first-child)
    (org-reveal '(4))))

;;;###autoload (autoload 'org-menu-visibility "org-menu" nil t)
(transient-define-prefix org-menu-visibility ()
  "A menu to control visibility of `org-mode' items."
  ["Visibility"
   ["Heading"
    ("a" "all" org-show-subtree :if-not org-at-block-p :transient t)
    ("a" "all" org-hide-block-toggle :if org-at-block-p :transient t)
    ("c" "cycle" org-cycle :transient t)
    ("t" "content" org-menu-show-headline-content :if-not org-at-block-p :transient t)
    ("h" "hide" outline-hide-subtree :if-not org-at-block-p :transient t)
    ("h" "hide" org-hide-block-toggle :if org-at-block-p :transient t)
    ("r" "reveal" (lambda () (interactive) (org-reveal t)) :if-not org-at-block-p :transient t)]
   ["Global"
    :pad-keys t
    ("C" "cycle global" org-global-cycle :transient t)
    ("go" "overview" org-overview)
    ("gt" "content" (lambda () (interactive) (org-content org-menu-global-toc-depth)))
    ("ga" "all" org-show-all)
    ("gd" "default" (lambda () (interactive) (org-set-startup-visibility)))]
   ["Narrow"
    :pad-keys t
    ("nn" "toggle" org-toggle-narrow-to-subtree)
    ("nb" "to block" org-narrow-to-block :if org-at-block-p)
    ("ns" "to sub tree" org-narrow-to-subtree)
    ("ne" "to element" org-narrow-to-element)
    ("w" "widen" widen)]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

(transient-define-prefix org-menu-visibility-columns ()
  "A menu to control visibility of `org-mode' items in `org-columns' mode."
  ["Visibility"
   ["Columns view"
    :if org-menu-show-columns-view-options-p
    ("t" "content" org-columns-content :transient t)
    ("o" "overview" org-overview :transient t)
    ("g" "refresh" org-columns-redo :transient t)]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

(defun org-menu-eval-src-items ()
  "Return the items to evaluate a source block."
  (list
   ["Source"
    :if org-menu-show-src-options-p
    ("e" "run block" org-babel-execute-src-block)
    ("c" "check headers" org-babel-check-src-block)
    ("k" "clear results" org-babel-remove-result-one-or-many)
    ("'" "edit" org-edit-special)]))

;;;###autoload (autoload 'org-menu-eval "org-menu" nil t)
(transient-define-prefix org-menu-eval ()
  "A menu to evaluate buffers, tables, etc. in `org-mode'."
  ["dummy"])

(defun org-menu-run-gnuplot ()
  "Will call `org-plot/gnuplot' and update inline images."
  (interactive)
  (org-plot/gnuplot)
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

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
    ["Plot"
     ("p" "gnuplot" org-menu-run-gnuplot)]
    ["Export"
     ("t" "tangle source files" org-babel-tangle)
     ("x" "export" org-export-dispatch)]
    ["Quit"
     :if-non-nil org-menu-use-q-for-quit
     ("q" "quit" transient-quit-all)]])

(defun org-menu-insert-block (str)
  "Insert an org mode block of type `STR'."
  (interactive)
  (insert (format "#+begin_%s\n#+end_%s\n" str str)))

(defun org-menu-insert-horizontal-rule ()
  "Insert a horizontal rule."
  (interactive)
  (insert "-----"))

;;;###autoload (autoload 'org-menu-insert-blocks "org-menu" nil t)
(transient-define-prefix org-menu-insert-blocks ()
  "A menu to insert new blocks in `org-mode'."
  [["Insert block"
    ("s" "source" (lambda () (interactive) (org-menu-insert-block "src")))
    ("e" "example" (lambda () (interactive) (org-menu-insert-block "example")))
    ("v" "verbatim" (lambda () (interactive) (org-menu-insert-block "verbatim")))
    ("a" "ascii" (lambda () (interactive) (org-menu-insert-block "ascii")))
    ("q" "quote" (lambda () (interactive) (org-menu-insert-block "quote")) :if-nil org-menu-use-q-for-quit)
    ("Q" "quote" (lambda () (interactive) (org-menu-insert-block "quote")) :if-non-nil org-menu-use-q-for-quit)
    ("d" "dynamic block" org-dynamic-block-insert-dblock)]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

;;;###autoload (autoload 'org-menu-insert-heading "org-menu" nil t)
(transient-define-prefix org-menu-insert-heading ()
  "A menu to insert new headings in `org-mode'."
  [["Heading"
    ("h" "heading" org-insert-heading)
    ("H" "heading (after)" org-insert-heading-after-current)
    ("T" "todo" org-insert-todo-heading)]
   ["Items"
    ("d" "drawer" org-insert-drawer)]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

;;;###autoload (autoload 'org-menu-insert-template "org-menu" nil t)
(transient-define-prefix org-menu-insert-template ()
  "A menu to insert new templates in `org-mode'."
  [["Templates"
    ("S" "structure template" org-insert-structure-template)
    ("B" "blocks" (lambda () (interactive) (org-menu-expand-snippet 'block)))
    ("O" "options" (lambda () (interactive) (org-menu-expand-snippet 'option)))]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

;;;###autoload (autoload 'org-menu-insert-timestamp "org-menu" nil t)
(transient-define-prefix org-menu-insert-timestamp ()
  "A menu to insert timestamps in Org Mode."
  [["Active"
    ("." "Time stamp" org-time-stamp)
    ("t" "Today" (lambda () (interactive) (org-insert-time-stamp (current-time) nil nil)))
    ("n" "Today + time" (lambda () (interactive) (org-insert-time-stamp (current-time) t nil)))]
   ["Inactive"
    ("!" "Time stamp (i)" org-time-stamp-inactive)
    ("T" "Today (i)" (lambda () (interactive) (org-insert-time-stamp (current-time) nil t)))
    ("N" "Today + time (i)" (lambda () (interactive) (org-insert-time-stamp (current-time) t t)))]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

(defun org-menu-table-insert-row-below ()
  "Insert a new table column below point."
  (interactive)
  (org-table-insert-row '4))

(defun org-menu-table-insert-column-left ()
  "Insert a new column to the left of point."
  (interactive)
  (org-table-insert-column)
  (org-table-move-column-right))

;;;###autoload (autoload 'org-menu-insert-table "org-menu" nil t)
(transient-define-prefix org-menu-insert-table ()
  "A menu to insert table items in `org-mode'."
  [["Table"
    ("t" "table" org-table-create-or-convert-from-region :if-not org-at-table-p)
    ("i" "import" org-table-import :if-not org-at-table-p)]
   ["Rows/columns"
    :if org-at-table-p
    ("r" "row above" org-table-insert-row :transient t)
    ("R" "row below" org-menu-table-insert-row-below :transient t)
    ("c" "column left" org-table-insert-column :transient t)
    ("C" "column right" org-menu-table-insert-column-left :transient t)
    ("-" "horiz. line" org-table-insert-hline :transient t)]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

(defun org-menu-insert-superscript ()
  "Insert a text with superscript."
  (interactive)
  (org-menu-expand-snippet 'superscript))

(defun org-menu-insert-subscript ()
  "Insert a text with subscript."
  (interactive)
  (org-menu-expand-snippet 'subscript))

(defun org-menu-parse-formatting (format-char)
  "Will return the bounds of the format markup `FORMAT-CHAR'."
  (let ((original-point (point))
        start end)
    (ignore-errors
      (save-excursion
        (save-restriction
          (save-match-data
            (org-narrow-to-element)
            (goto-char (search-backward (format "%c" format-char)))
            (setq start (point))
            (goto-char original-point)
            (goto-char (search-forward (format "%c" format-char)))
            (setq end (point))
            (cons start end)))))))

(defun org-menu-toggle-format (format-char)
  "Will either remove `FORMAT-CHAR' or add it around region/point."
  (let ((range (org-menu-parse-formatting format-char))
        (format-string (format "%c" format-char)))
    (if (null range)
        (org-menu-insert-text format-string format-string t)
      (goto-char (cdr range))
      (delete-char -1)
      (goto-char (car range))
      (delete-char 1))))

;;;###autoload (autoload 'org-menu-insert-list "org-menu" nil t)
(transient-define-prefix org-menu-insert-list ()
  "A menu to insert lists."
  [["List"
    ("-" "item" (lambda () (interactive) (insert "- ")))
    ("+" "+" (lambda () (interactive) (insert "+ ")))
    ("*" "*" (lambda () (interactive) (insert "* ")))
    ("." "1." (lambda () (interactive) (insert "1. ")))
    (")" "1)" (lambda () (interactive) (insert "1) ")))]
   ["Todo"
    ("t" "todo" (lambda () (interactive) (insert "- [ ] ")))
    ("d" "done" (lambda () (interactive) (insert "- [X] ")))
    ("p" "partial" (lambda () (interactive) (insert "- [-] ")))]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

(defun org-menu-insert-plot ()
  "Insert a small example plot for `gnu-plot'."
  (interactive)
  (beginning-of-line 1)
  (org-menu-expand-snippet 'plot))

(defun org-menu-insert-option-line-smart (line)
  "Insert `LINE'.  If inside a block move to right before it."
  (beginning-of-line 1)
  (insert line "\n"))

(defun org-menu-insert-name (name)
  "Insert a #+NAME for the next element."
  (interactive "MName? ")
  (org-menu-insert-option-line-smart (format "#+NAME: %s" name)))

(defun org-menu-insert-caption (caption)
  "Insert a #+CAPTION for the next element."
  (interactive "MCaption? ")
  (org-menu-insert-option-line-smart (format "#+CAPTION: %s" caption)))

(defun org-menu-insert-startup-setting (setting)
  "Insert a buffer `SETTING'."
  (interactive (list (completing-read "Startup setting? "
                                (mapcar 'car org-startup-options))))
  (org-menu-insert-option-line-smart (format "#+STARTUP: %s" setting)))

(defun org-menu-insert-buffer-setting (setting)
  "Insert a buffer `SETTING'."
  (interactive (list (completing-read "Buffer setting? " org-options-keywords)))
  (insert (format "#+%s " setting)))

(defun org-menu-insert-footnote-definition (name definition)
  "Insert a definition for a footnote.

Named `NAME' using `DEFINITION'."
  (interactive "MName? \nMDefinition? ")
  (org-menu-insert-option-line-smart (format "[fn:%s] %s" name definition)))

(defun org-menu-insert-footnote-inline (name definition)
  "Insert a definition for an inline footnote.

Named `NAME' with `DEFINITION'."
  (interactive "MName? \nMDefinition? ")
  (insert (format "[fn:%s: %s]" name definition)))

;;;###autoload (autoload 'org-menu-insert "org-menu" nil t)
(transient-define-prefix org-menu-insert ()
  "A menu to insert new items in `org-mode'."
  ["Insert"
   ["Element"
    ("." "time" org-menu-insert-timestamp)
    ("l" "link (new)" org-insert-link)
    ("L" "link (stored)" org-insert-last-stored-link :transient t)
    ("T" "templates" org-menu-insert-template)]
   ["Structure"
    ("h" "heading" org-menu-insert-heading)
    ("-" "list" org-menu-insert-list)
    ("H" "hor. rule" org-menu-insert-horizontal-rule)]
   ["Block/table"
    ("b" "block" org-menu-insert-blocks)
    ("t" "table" org-menu-insert-table)
    ("p" "plot" org-menu-insert-plot)]
   ["Format"
    ("^" "superscript" org-menu-insert-superscript)
    ("_" "subscript" org-menu-insert-subscript)]
   ["Footnotes"
    ("fd" "define" org-menu-insert-footnote-definition)
    ("fi" "inline" org-menu-insert-footnote-inline)]
   ["Options"
    ("n" "name" org-menu-insert-name)
    ("c" "caption" org-menu-insert-caption)
    ("s" "startup option" org-menu-insert-startup-setting)
    ("o" "buffer option" org-menu-insert-buffer-setting)]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

(defun org-menu-comment-line ()
  "Toggle line comment w/o moving cursor."
  (interactive)
  (save-excursion (comment-line 1)))

(defun org-menu-fix-timestamp ()
  "Fix the timestamp at `(point)'."
  (interactive)
  (org-timestamp-change 0 'day))

(defun org-menu-insert-text (left right &optional surround-whitespace)
  "Will insert left|right and put the curser at |.

If region is active it will be surrounded by `LEFT' and `RIGHT' and
the point will be at end of region.  Will add spaces before/after text if
`SURROUND-WHITESPACE' is true and it's needed."
  (let ((start (point))
        (end (point)))
    (when (region-active-p)
      (setq start (region-beginning)
            end (region-end))
      (deactivate-mark))
    (when (> start end)
      (cl-psetq start end
                end start))

    (goto-char start)
    (when (and surround-whitespace
               (not (bolp))
               (not (looking-back " +" nil)))
      (insert " "))
    (insert left)

    (forward-char (- end start))

    (save-excursion
      (insert right)
      (when (and surround-whitespace
                 (not (eolp))
                 (not (looking-at " +")))
        (insert " ")))))

;;;###autoload (autoload 'org-menu-goto "org-menu" nil t)
(transient-define-prefix org-menu-goto ()
  "Menu to go to different places by name."
  [["Go to"
    ("h" "heading" imenu)
    ("s" "source block" org-babel-goto-named-src-block)
    ("r" "result block" org-babel-goto-named-result)
    ("." "calendar" org-goto-calendar :if org-menu-show-timestamp-options-p)]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

(defun org-menu-toggle-zwspace ()
  "Will remove zero-width space before/after point or insert it if none found."
  (interactive)
  (let ((zww (eval-when-compile (string (char-from-name "ZERO WIDTH SPACE")))))
    (save-excursion
      (skip-chars-backward zww)
      (if (looking-at (rx (+ (literal zww))))
      (replace-match "")
    (insert zww)))))

(defun org-menu-text-format-items (check-for-table)
  "Items to format text.

Will add an ':if org-menu-show-text-options-p' criteria if
`CHECK-FOR-TABLE' is true."
  (list
   `["Navigate"
     ,@(when check-for-table '(:if org-menu-show-text-options-p))
     :pad-keys t
     ("p" "up" previous-line :transient t)
     ("n" "down" next-line :transient t)
     ("b" "left" backward-word :transient t)
     ("f" "right" forward-word :transient t)
     ("u" "parent" org-up-element :transient t)
     ("M-w" "store link" org-store-link :transient t :if-not region-active-p)
     ("C-_" "undo" undo :transient t)
     ("SPC" "mark" set-mark-command :transient t)
     ("C-x C-x" "exchange" exchange-point-and-mark :transient t)]
   `["Formatting"
     ,@(when check-for-table '(:if org-menu-show-text-options-p))
     :pad-keys t
     ("*" "Bold" (lambda nil (interactive) (org-menu-toggle-format ?*)) :transient t)
     ("/" "italic" (lambda nil (interactive) (org-menu-toggle-format ?/)) :transient t)
     ("_" "underline" (lambda nil (interactive) (org-menu-toggle-format ?_)) :transient t)
     ("+" "strikethrough" (lambda nil (interactive) (org-menu-toggle-format ?+)) :transient t)
     ("S-SPC" "non-breaking space" org-menu-toggle-zwspace :transient t)]
   `["Source"
     ,@(when check-for-table '(:if org-menu-show-text-options-p))
     ("~" "code" (lambda nil (interactive) (org-menu-toggle-format ?~)) :transient t)
     ("=" "verbatim" (lambda nil (interactive) (org-menu-toggle-format ?=)) :transient t)]))

;;;###autoload (autoload 'org-menu-text-in-element "org-menu" nil t)
(transient-define-prefix org-menu-text-in-element ()
  "Add formatting for text inside other elements like lists and tables."
  ["dummy"])

(transient-insert-suffix 'org-menu-text-in-element (list 0)
  `[,@(org-menu-text-format-items nil)
    ["Quit"
     :if-non-nil org-menu-use-q-for-quit
     ("q" "quit" transient-quit-all)]])

;;;###autoload (autoload 'org-menu-options "org-menu" nil t)
(transient-define-prefix org-menu-options ()
  "A menu to toggle options."
  [["Display"
    ("l" "show links" org-toggle-link-display)
    ("i" "inline images" org-toggle-inline-images)
    ("p" "pretty entities" org-toggle-pretty-entities)
    ("I" "indent by level" org-indent-mode)
    ("t" "timestamp overlay" org-toggle-time-stamp-overlays)
    ("n" "numbered headings" org-num-mode)]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

(defun org-menu-toggle-has-checkbox ()
  "Toggle whether the current list item has a checkbox."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (if (not (looking-at "- "))
        (message "Not at list item")
      (end-of-line 1)
      (org-ctrl-c-ctrl-c '(4)))))

(defun org-menu-is-timer-running ()
  "Return whether a timer is currently running."
  (and org-timer-start-time
       (not org-timer-countdown-timer)
       (not org-timer-pause-time)))

(defun org-menu-is-timer-paused ()
  "Return whether a timer has been started and is paused."
  (and org-timer-start-time
       (not org-timer-countdown-timer)
       org-timer-pause-time))

;;;###autoload (autoload 'org-menu-clock "org-menu" nil t)
(transient-define-prefix org-menu-clock ()
  "Time management using org-modes clock."
  [["Clock"
    :pad-keys t
    ("<tab>" "in" org-clock-in :if-not org-clock-is-active)
    ("TAB" "in" org-clock-in :if-not org-clock-is-active)
    ("o" "out" org-clock-out :if org-clock-is-active)
    ("j" "goto" org-clock-goto :if org-clock-is-active)
    ("q" "cancel" org-clock-cancel
     :if (lambda () (and (not org-menu-use-q-for-quit)
                         (org-clock-is-active))))
    ("Q" "cancel" org-clock-cancel
     :if (lambda () (and org-menu-use-q-for-quit
                         (org-clock-is-active))))
    ("d" "display" org-clock-display :if org-clock-is-active)
    ("x" "in again" org-clock-in-last :if-not org-clock-is-active)
    ("z" "resolve" org-resolve-clocks)]
   ["Timer"
    ("0" "start" org-timer-start :if-nil org-timer-start-time)
    ("_" "stop" org-timer-stop :if-non-nil org-timer-start-time)
    ("." "insert" org-timer :if-non-nil org-timer-start-time)
    ("-" "... item" org-timer-item :if-non-nil org-timer-start-time)
    ("," "pause" org-timer-pause-or-continue :if org-menu-is-timer-running)
    ("," "continue" org-timer-pause-or-continue :if org-menu-is-timer-paused)
    (";" "countdown" org-timer-set-timer :if-nil org-timer-start-time)]
   ["Effort"
    ("e" "set effort" org-set-effort)
    ("E" "increase" org-inc-effort)]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

(defun org-menu-columns-globally ()
  "Turn on `org-columns' globally."
  (interactive)
  (org-columns t))

(transient-define-prefix org-menu-search-and-filter ()
  "A menu to search and filter `org-mode' documents."
  ["Search and filter"
   ["Filter"
    ("/" "only matching" org-sparse-tree)
    ("q" "tags" org-tags-sparse-tree :if-nil org-menu-use-q-for-quit)
    ("Q" "tags" org-tags-sparse-tree :if-non-nil org-menu-use-q-for-quit)
    ("t" "todos" org-show-todo-tree)
    ("d" "deadlines" org-check-deadlines)
    ("r" "remove highlights" org-remove-occur-highlights :if-non-nil org-occur-highlights)]
   ["Dates"
    ("b" "before" org-check-before-date)
    ("a" "after" org-check-after-date)
    ("D" "range" org-check-dates-range)]
   ["Views"
    ("A" "agenda" org-agenda)
    ("c" "columns" org-columns :if-nil org-columns-current-fmt)
    ("c" "columns off" org-columns-quit :if-non-nil org-columns-current-fmt)
    ("gc" "whole buffer" org-menu-columns-globally :if-nil org-columns-current-fmt)]
   ["Quit"
    :if-non-nil org-menu-use-q-for-quit
    ("q" "quit" transient-quit-all)]])

(transient-define-prefix org-menu-attachments ()
  "A menu to manage attachments."
  ["Attachments"
   ["Add"
    ("a" "file" org-attach-attach)
    ("c" "copy" org-attach-attach-cp)
    ("m" "move" org-attach-attach-mv)
    ("l" "link" org-attach-attach-ln)
    ("y" "symlink" org-attach-attach-lns)
    ("u" "download" org-attach-url)
    ("b" "buffer" org-attach-buffer)
    ("n" "new" org-attach-new)]
   ["Open"
    ("o" "attachment" org-attach-open)
    ("O" "in Emacs" org-attach-open-in-emacs)
    ("f" "directory" org-attach-reveal)
    ("F" "in Emacs" org-attach-reveal-in-emacs)]
   ["Delete"
    ("d" "delete" org-attach-delete-one)
    ("D" "all" org-attach-delete-all)]
   ["More"
    ("s" "set directory" org-attach-set-directory)
    ("S" "unset" org-attach-unset-directory)
    ("z" "synchronize" org-attach-sync)]]
  (interactive)
  (require 'org-attach)
  (transient-setup 'org-menu-attachments))

(transient-define-prefix org-menu-archive ()
  "A menu to archive items."
  ["dummy"])

(transient-insert-suffix 'org-menu-archive (list 0)
  `["Archive"
    ,@(org-menu-heading-navigate-items nil #'org-cycle-force-archived)
    ["Archive to"
     ("t" "tree" org-archive-subtree :transient t)
     ("s" "sibling" org-archive-to-archive-sibling :transient t)
     ("Q" "tag" org-toggle-archive-tag :transient t)]])

(defun org-menu-insert-todo-heading-after-current ()
  "Insert a new todo heading with same level as current, after subtree."
  (interactive)
  (org-insert-todo-heading '(16)))

;;;###autoload (autoload 'org-menu "org-menu" nil t)
(transient-define-prefix org-menu ()
  "A discoverable menu to edit and view `org-mode' documents."
  ["dummy"])

(transient-insert-suffix 'org-menu (list 0)
  `["Org mode"
    ;; Items for headings
    ,@(org-menu-heading-navigate-items t)

    ["Move heading"
     :if org-menu-show-heading-options-p
     ("P" "up" org-metaup :transient t)
     ("N" "down" org-metadown :transient t)
     ("B" "left" org-shiftmetaleft :transient t)
     ("F" "right" org-shiftmetaright :transient t)
     ("b" "left (line)" org-metaleft :transient t)
     ("f" "right (line)" org-metaright :transient t)
     ("r" "refile" org-refile :transient t)]
    ["Change heading"
     :if org-menu-show-heading-options-p
     ("*" "toggle" org-ctrl-c-star :if-not org-at-table-p :transient t)
     ("t" "todo" org-todo :transient t)
     ("q" "tags" org-set-tags-command :transient t :if-nil org-menu-use-q-for-quit)
     ("Q" "tags" org-set-tags-command :transient t :if-non-nil org-menu-use-q-for-quit)
     ("y" "property" org-set-property :transient t)
     ("," "priority" org-priority :transient t)
     ("A" "archive" org-menu-archive :transient t)
     ("D" "deadline" org-deadline :transient t)
     ("S" "schedule" org-schedule :transient t)
     ("/" "comment" org-toggle-comment :transient t)
     ("N" "add note" org-add-note)]
    ["Make new/delete"
     :if org-menu-show-heading-options-p
     :pad-keys t
     ("mh" "make heading (before)" org-insert-heading)
     ("mH" "make heading (after)" org-insert-heading-after-current)
     ("mt" "make todo (before)" org-insert-todo-heading)
     ("mT" "make todo (after)" org-menu-insert-todo-heading-after-current)
     ("mc" "clone with time shift" org-clone-subtree-with-time-shift)
     ("dh" "delete heading" org-cut-subtree :transient t)
     ("dy" "delete property" org-delete-property :transient t)
     ("a" "attachments" org-menu-attachments)
     ("C-w" "cut tree" org-cut-special :transient t)
     ("C-y" "yank tree" org-paste-special :transient t)]

    ;; Items for tables
    ["Navigate"
     :if org-menu-show-table-options-p
     :pad-keys t
     ("p" "up" previous-line :transient t)
     ("n" "down" next-line :transient t)
     ("b" "left" org-table-previous-field :transient t)
     ("f" "right" org-table-next-field :transient t)
     ("u" "parent" outline-up-heading :transient t)
     ("M-w" "store link" org-store-link :transient t :if-not region-active-p)
     ("C-_" "undo" undo :transient t)]
    ["Move r/c"
     :if org-menu-show-table-options-p
     ("P" "up" org-table-move-row-up :transient t)
     ("N" "down" org-table-move-row-down :transient t)
     ("B" "left" org-table-move-column-left :transient t)
     ("F" "right" org-table-move-column-right :transient t)]
    ["Field"
     :if org-menu-show-table-options-p
     :pad-keys t
     ("'" "edit" org-table-edit-field)
     ("SPC" "blank" org-table-blank-field :transient t)
     ("RET" "from above" org-table-copy-down :transient t)
     ("t" "text formatting" org-menu-text-in-element)]
    ["Formulas"
     :if org-menu-show-table-options-p
     ("E" "edit all" org-table-edit-formulas :transient t)
     ("=" "field" (lambda () (interactive) (org-table-eval-formula '(4))) :transient t)
     ("+" "in place" (lambda () (interactive) (org-table-eval-formula '(16))))
     ("c" "column" org-table-eval-formula :transient t)
     ("h" "coordinates" org-table-toggle-coordinate-overlays :transient t)
     ("D" "debug" org-table-toggle-formula-debugger :transient t)]
    ["Table"
     :if org-menu-show-table-options-p
     :pad-keys t
     ("dr" "delete row" org-shiftmetaup :transient t)
     ("dc" "delete column" org-shiftmetaleft :transient t)
     ("m" "make" org-menu-insert-table)
     ,@(when (fboundp (function org-table-toggle-column-width))
         ;; This will emit a warning during byte compilation. We can ignore it
         (list '("S" "shrink column" org-table-toggle-column-width :transient t)))
     ("r" "sort" org-table-sort-lines :transient t)
     ("M-w" "copy rect" org-table-copy-region :transient t :if region-active-p)
     ("C-w" "cut rect" org-table-cut-region :transient t :if region-active-p)
     ("C-y" "yank rect" org-table-paste-rectangle :transient t)]

    ;; Items for lists
    ["Navigate"
     :if org-menu-show-list-options-p
     :pad-keys t
     ("p" "prev" previous-line :transient t)
     ("n" "next" next-line :transient t)
     ("c" "cycle" org-cycle :transient t)
     ("u" "parent" org-up-element :transient t)
     ("M-p" "prev (same level)" org-backward-element :transient t)
     ("M-n" "next (same level)" org-forward-element :transient t)
     ("M-w" "store link" org-store-link :transient t :if-not region-active-p)
     ("C-_" "undo" undo :transient t)]
    ["Move list"
     :if org-menu-show-list-options-p
     ("P" "up" org-metaup :transient t)
     ("N" "down" org-metadown :transient t)
     ("B" "left" org-shiftmetaleft :transient t)
     ("F" "right" org-shiftmetaright :transient t)
     ("b" "left (line)" org-metaleft :transient t)
     ("f" "right (line)" org-metaright :transient t)]
    ["List"
     :if org-menu-show-list-options-p
     ("R" "repair" org-list-repair)
     ("*" "turn into tree" org-list-make-subtree)
     ("S" "sort" org-sort-list :transient t)
     ("t" "text formatting" org-menu-text-in-element)]
    ["Toggle"
     :if org-menu-show-list-options-p
     ("-" "list item" org-toggle-item :if-not org-at-table-p :transient t)
     ("+" "list style" org-cycle-list-bullet :if-not org-at-table-p :transient t)
     ("d" "done" org-toggle-checkbox :transient t)
     ("h" "half-done"
      (lambda () (interactive) (org-toggle-checkbox '(16)))
      :transient t)
     ("m" "checkbox" org-menu-toggle-has-checkbox :transient t)]

    ;; Items for text
    ,@(org-menu-text-format-items t)
    ["Line"
     :if org-menu-show-text-options-p
     :pad-keys t
     (":" "fixed width" org-toggle-fixed-width :transient t)
     (";" "comment" org-menu-comment-line :transient t)
     ("--" "list" org-toggle-item :transient t)
     ("-*" "heading" org-ctrl-c-star :transient t)]

    ;; Items for source blocks
    ,@(org-menu-eval-src-items)

    ["Link"
     :if org-menu-show-link-options-p
     ("e" "edit" org-insert-link :transient t)]

    ["Timestamp"
     :if org-menu-show-timestamp-options-p
     ("." "type" org-toggle-timestamp-type :transient t)
     ("e" "edit" org-time-stamp :transient t)
     ("R" "repair" org-menu-fix-timestamp :transient t)]

    ["Footnote"
     :if org-menu-show-footnote-options-p
     ("ed" "delete" (lambda () (interactive) (org-footnote-delete)))
     ("es" "sort" (lambda () (interactive) (org-footnote-sort)))
     ("er" "renumber" (lambda () (interactive) (org-footnote-renumber-fn:N)))
     ("eS" "sort+renumber" (lambda () (interactive)
                (org-footnote-renumber-fn:N)
                (org-footnote-sort)))
     ("en" "normalize" (lambda () (interactive) (org-footnote-normalize)))]

    ;; Items for column view
    ["Navigate"
     :if org-menu-show-columns-view-options-p
     :pad-keys t
     ("p" "prev" org-columns-move-up :transient t)
     ("n" "next" org-columns-move-down :transient t)
     ("f" "forward" forward-char :transient t)
     ("b" "backward" backward-char :transient t)
     ("M-w" "store link" org-store-link :transient t :if-not region-active-p)
     ("C-_" "undo" undo :transient t)]
    ["Value"
     :if org-menu-show-columns-view-options-p
     :pad-keys t
     ("e" "edit" org-columns-edit-value :transient t)
     ("V" "show" org-columns-show-value :transient t)
     ("M-n" "next" org-columns-next-allowed-value :transient t)
     ("M-p" "previous" org-columns-previous-allowed-value :transient t)
     ("a" "edit allowed" org-columns-edit-allowed :transient t)]
    ["Column"
     :if org-menu-show-columns-view-options-p
     :pad-keys t
     ("E" "edit column" org-columns-edit-attributes :transient t)
     ("{" "narrow" org-columns-narrow :transient t)
     ("}" "widen" org-columns-widen :transient t)
     ("M-<right>" "move right" org-columns-move-right :transient t)
     ("M-<left>" "move left" org-columns-move-left :transient t)
     ("M-S-<right>" "new" org-columns-new :transient t)
     ("M-S-<left>" "delete" org-columns-delete :transient t)]

    ["Tasks"
     ("v" "visibility" org-menu-visibility :if-not org-menu-show-columns-view-options-p)
     ("v" "visibility" org-menu-visibility-columns :if org-menu-show-columns-view-options-p)
     ("x" "evaluation" org-menu-eval)
     ("i" "insert" org-menu-insert)
     ("g" "go to" org-menu-goto)
     ("s" "search" org-menu-search-and-filter)
     ("o" "options" org-menu-options)
     ("C" "clock (active)" org-menu-clock :if org-clock-is-active)
     ("C" "clock" org-menu-clock :if-not org-clock-is-active)
     ,@(when (fboundp #'org-capture-finalize)
         (list '("C-c C-c" "confirm capture" org-capture-finalize :if-non-nil org-capture-mode)))
     ,@(when (fboundp #'org-capture-kill)
         (list '("C-c C-k" "abort capture" org-capture-kill :if-non-nil org-capture-mode)))
     ""
     ("q" "quit" transient-quit-all :if-non-nil org-menu-use-q-for-quit)]])

(provide 'org-menu)
;;; org-menu.el ends here
