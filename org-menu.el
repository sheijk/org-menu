;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transient interface for org

(transient-define-prefix shk-org-menu-visibility ()
  "A menu to control visibility of org-mode items"
  ["Visibility"
   ["Visibility"
    ("c" "cycle" org-cycle :transient t)
    ("a" "all" org-show-subtree :if-not org-at-block-p :transient t)
    ("a" "all" org-hide-block-toggle :if org-at-block-p :transient t)
    ("t" "content" shk-org-show-content :if-not org-at-block-p :transient t)
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

(transient-define-prefix shk-org-menu-eval ()
  "A menu to evaluate buffers, tables, etc. in org-mode"
  ["Evaluation"
   ["Table"
    :if org-at-table-p
    ("e" "table" (lambda () (interactive) (org-table-recalculate 'iterate)))
    ("1" "one iteration" (lambda () (interactive) (org-table-recalculate t)))
    ("l" "line" (lambda () (interactive) (org-table-recalculate nil)))
    ("f" "format" org-table-align :if org-at-table-p)]
   ["Source"
    :if org-in-src-block-p
    ("e" "run block" org-babel-execute-src-block)
    ("c" "check headers" org-babel-check-src-block)
    ("k" "clear results" org-babel-remove-result-one-or-many)]
   ["Headline"
    :if-not org-in-src-block-p
    ("c" "update checkbox count" org-update-checkbox-count)]
   [("q" "quit" transient-quit-all)]])

(defun shk-org-menu-insert-block (str)
  "Insert an org mode block of type `str'"
  (interactive)
  (insert (format "#+begin_%s\n#+end_%s\n" str str)))

(defun shk-org-menu-expand-snippet (snippet)
  "Will expand the given snippet."
  (interactive)
  (insert snippet)
  (yas-expand))

(transient-define-prefix shk-org-menu-insert ()
  "A menu to insert new items in org-mode"
  ["Insert"
   ["Time"
    ("." "time stamp" org-time-stamp)
    ("!" "inactive" org-time-stamp-inactive)
    ("t." "now" (lambda () (interactive) (org-insert-time-stamp (current-time) t)))
    ("t!" "now (inactive)" (lambda () (interactive) (org-insert-time-stamp (current-time) t)))]
   ["Blocks"
    ("bs" "source" (lambda () (interactive) (shk-org-menu-insert-block "src")))
    ("be" "example" (lambda () (interactive) (shk-org-menu-insert-block "example")))
    ("bv" "verbatim" (lambda () (interactive) (shk-org-menu-insert-block "verbatim")))
    ("ba" "ascii" (lambda () (interactive) (shk-org-menu-insert-block "ascii")))
    ("bq" "quote" (lambda () (interactive) (shk-org-menu-insert-block "quote")))
    ("bd" "dynamic block" org-insert-dblock)]
   ["More items"
    ("md" "drawer" org-insert-drawer)
    ("mi" "item" org-insert-item)]
   ["Headline"
    ("mh" "headline" org-insert-heading)
    ("mH" "headline (after)" org-insert-heading-after-current)
    ("mt" "todo" org-insert-todo-heading)]
   ["Templates"
    ("S" "structure template" org-insert-structure-template)
    ("B" "yas blocks" (lambda () (interactive) (shk-org-menu-expand-snippet "beg")))
    ("O" "yas options" (lambda () (interactive) (shk-org-menu-expand-snippet "opt")))]
   [("q" "quit" transient-quit-all)]])

;; shk-org-menu-insert has no need for Navigation sub menu

(defun shk-org-menu-comment-line ()
  "Toggles line comment w/o moving cursor"
  (interactive)
  (save-excursion (comment-line 1)))

(defun shk-org-menu-insert-text (left right)
  "Will insert left|right and put the curser at |"
  (if (region-active-p)
      (progn
        (insert left)
        (exchange-point-and-mark)
        (insert right))
    (insert left)
    (insert right)
    (backward-char (length right))))

(transient-define-prefix shk-org-menu-goto ()
  "Menu to go to different places by name"
  ["Go to"
   ("s" "goto source block" org-babel-goto-named-src-block)
   ("r" "goto result block" org-babel-goto-named-result)
   ("h" "goto heading" imenu)]
  [("q" "quit" transient-quit-all)])

(defun shk-org-menu-at-text-p ()
  "Returns whether point is at text"
  (not (or (org-at-heading-p)
           (org-at-table-p)
           (org-in-item-p)
           (org-in-src-block-p))))

(defun shk-org-menu-text-format-items (check-for-table)
  (list
   `["Formatting"
     ,@(if (not check-for-table)
           nil
         '(:if shk-org-menu-at-text-p))
     ("*" "Bold" (lambda nil (interactive) (shk-org-menu-insert-text "*" "*")))
     ("/" "italic" (lambda nil (interactive) (shk-org-menu-insert-text "/" "/")))
     ("_" "underline" (lambda nil (interactive) (shk-org-menu-insert-text "_" "_")))
     ("+" "strikethrough" (lambda nil (interactive) (shk-org-menu-insert-text "+" "+")))]
   `["Source"
     ,@(if (not check-for-table)
           nil
         '(:if shk-org-menu-at-text-p))
     ("~" "code" (lambda nil (interactive) (shk-org-menu-insert-text "~" "~")))
     ("=" "verbatim" (lambda nil (interactive) (shk-org-menu-insert-text "=" "=")))]))

(transient-define-prefix shk-org-menu-text-in-list ()
  "Add formatting for text in lists"
  ["dummy"])

(transient-insert-suffix 'shk-org-menu-text-in-list (list 0)
  ;; These sub menus have a similar version in shk-org-menu, keep in sync
  `[,@(shk-org-menu-text-format-items nil)
    [("q" "quit" transient-quit-all)]])

(transient-define-prefix shk-org-menu ()
  "A discoverable menu to edit and view org-mode documents"
  ["dummy"])

(transient-insert-suffix 'shk-org-menu (list 0)
  `["Org mode"
    ["Navigate"
     :if org-at-heading-p
     ("p" "prev" org-previous-visible-heading :transient t)
     ("n" "next" org-next-visible-heading :transient t)
     ("c" "cycle" org-cycle :transient t)
     ("u" "parent" outline-up-heading :transient t)
     ("M-P" "prev (same level)" shk-org-prev-heading :transient t)
     ("M-N" "next (same level)" shk-org-next-heading :transient t)
     ("'" "by name" imenu :transient t)]
    ["Move headline"
     :if org-at-heading-p
     ("P" "up" org-metaup :transient t)
     ("N" "down" org-metadown :transient t)
     ("B" "left" org-shiftmetaleft :transient t)
     ("F" "right" org-shiftmetaright :transient t)
     ("b" "left (line)" org-metaleft :transient t)
     ("f" "right (line)" org-metaright :transient t)
     ("r" "refile" org-refile :transient t)]
    ["Change headline"
     :if org-at-heading-p
     ("*" "toggle" org-ctrl-c-star :if-not org-at-table-p :transient t)
     ("t" "todo" org-todo :transient t)
     ("T" "tags" org-set-tags-command :transient t)
     ("A" "archive" org-toggle-archive-tag :transient t)
     ("/" "comment" org-toggle-comment :transient t)
     ("C-w" "cut tree" org-cut-special :transient t)
     ("C-y" "yank tree" org-paste-special :transient t)]
    ["Make new"
     :if org-at-heading-p
     ("mh" "headline (before)" org-insert-heading)
     ("mH" "headline (after)" org-insert-heading-after-current)
     ("mt" "todo (before)" org-insert-todo-heading)]

    ["Navigate"
     :if org-at-table-p
     ("p" "up" previous-line :transient t)
     ("n" "down" next-line :transient t)
     ("b" "left" org-table-previous-field :transient t)
     ("f" "right" org-table-next-field :transient t)]
    ["Move r/c"
     :if org-at-table-p
     ("P" "up" org-table-move-row-up :transient t)
     ("N" "down" org-table-move-row-down :transient t)
     ("B" "left" org-table-move-column-left :transient t)
     ("F" "right" org-table-move-column-right :transient t)]
    ["Create/delete"
     :if org-at-table-p
     ("mr" "row above" org-table-insert-row :transient t)
     ("mc" "column right" org-table-insert-column :transient t)
     ("m-" "horiz. line" org-table-insert-hline :transient t)
     ("dr" "delete row" org-shiftmetaup :transient t)
     ("dc" "delete column" org-shiftmetaleft :transient t)
     ("mt" "table from region" org-table-create-or-convert-from-region)]
    ["Formulas"
     :if org-at-table-p
     ("e" "edit all" org-table-edit-formulas :transient t)
     ("=" "field" (lambda () (interactive) (org-table-eval-formula '(4))) :transient t)
     ("+" "in place" (lambda () (interactive) (org-table-eval-formula '(16))))
     ("c" "column" org-table-eval-formula :transient t)
     ("h" "coordinates" org-table-toggle-coordinate-overlays :transient t)
     ("D" "debug" org-table-toggle-formula-debugger :transient t)]
    ["Field"
     :if org-at-table-p
     ("E" "edit" org-table-edit-field :transient t)
     ("SPC" "blank" org-table-blank-field :transient t)
     ("RET" "from above" org-table-copy-down :transient t)]
    ["Table"
     :if org-at-table-p
     ("S" "shrink column" org-table-toggle-column-width :transient t)]

    ["Navigation"
     :if org-in-item-p
     ("C-p" "prev" previous-line
      :if-not (lambda () transient-detect-key-conflicts)
      :transient t)
     ("M-p" "prev, (fallback)" previous-line
      :if (lambda () transient-detect-key-conflicts)
      :transient t)
     ("C-n" "next" next-line :transient t)
     ("c" "cycle" org-cycle :transient t)
     ("u" "parent" org-up-element :transient t)
     ("p" "prev (same level)" org-backward-element :transient t)
     ("n" "next (same level)" org-forward-element :transient t)]
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
     ("t" "text formatting" shk-org-menu-text-in-list)]
    ["Toggle"
     :if org-in-item-p
     ("-" "list item" org-toggle-item :if-not org-at-table-p :transient t)
     ("+" "list style" org-cycle-list-bullet :if-not org-at-table-p :transient t)]

    ["Line"
     :if shk-org-menu-at-text-p
     (":" "fixed width" org-toggle-fixed-width :transient t)
     (";" "comment" shk-org-menu-comment-line :transient t)]
    ,@(shk-org-menu-text-format-items t)

    ["Tasks"
     ("v" "visibility" shk-org-menu-visibility)
     ("x" "evaluation" shk-org-menu-eval)
     ("i" "insert" shk-org-menu-insert)
     ("g" "go to" shk-org-menu-goto)
     ""
     ("q" "quit" transient-quit-all)]])
