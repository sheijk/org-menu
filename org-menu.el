;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transient interface for org

(defvar shk-org-menu-navigation-items
  '(("p" "prev" org-previous-visible-heading :transient t)
    ("n" "next" org-next-visible-heading :transient t)
    ("c" "cycle" org-cycle :transient t)
    ("u" "parent" outline-up-heading :transient t)
    ("P" "prev (same level)" shk-org-prev-heading :transient t)
    ("N" "next (same level)" shk-org-next-heading :transient t)
    ("'" "by name" imenu :transient t))
  "Items which gets inserted into all commands adding navigation commands")

(defun shk-org-menu-add-navigation-items (prefix)
  "Add navigation items unless their key is already being used."

  (transient-insert-suffix prefix (list 0 0)
    `["Navigate"
      ,@(seq-filter
         (lambda (item)
           (let* ((key (car item)))
             (not (ignore-errors (transient-get-suffix prefix (vector key))))))
         shk-org-menu-navigation-items)]))

(transient-define-prefix shk-org-menu-structure ()
  "A menu to change the org-mode structure"
  ["Edit document structure"
   ["Move headline"
    ("P" "move up" org-metaup :transient t)
    ("N" "move down" org-metadown :transient t)
    ("F" "indent" org-shiftmetaright :transient t)
    ("B" "unindent" org-shiftmetaleft :transient t)
    ("f" "indent line" org-metaright :transient t)
    ("b" "unindent line" org-metaleft :transient t)
    ("r" "refile" org-refile :transient t)]
   ["Headline"
    ("t" "todo" org-todo :transient t)
    ("T" "tags" org-set-tags-command :transient t)
    ("A" "archive" org-toggle-archive-tag :transient t)
    ("/" "comment" org-toggle-comment :transient t)
    ("C-w" "cut tree" org-cut-special :transient t)
    ("C-y" "yank tree" org-paste-special :transient t)]
   ["Make new"
    ("mh" "headline" org-insert-heading)
    ("mH" "headline (after)" org-insert-heading-after-current)
    ("mt" "todo" org-insert-todo-heading)
    ("md" "drawer" org-insert-drawer)
    ("mi" "item" org-insert-item)
    ("mD" "dynamic block" org-insert-dblock)
    ("mS" "structure" org-insert-structure-template)]
   ["Toggle"
    ("-" "list item" org-toggle-item :if-not org-at-table-p)
    ("+" "list style" org-cycle-list-bullet :if-not org-at-table-p :transient t)
    ("*" "headline" org-ctrl-c-star :if-not org-at-table-p)
    (":" "fixed width" org-toggle-fixed-width)]
   [("q" "quit" transient-quit-all)]])

(shk-org-menu-add-navigation-items 'shk-org-menu-structure)

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

(shk-org-menu-add-navigation-items 'shk-org-menu-visibility)

(transient-define-prefix shk-org-menu-eval ()
  "A menu to evaluate buffers, tables, etc. in org-mode"
  ["Evaluation"
   ["Table"
    :if org-at-table-p
    ("e" "table" (lambda () (interactive) (org-table-recalculate 'iterate)))
    ("1" "one iteration" (lambda () (interactive) (org-table-recalculate t)))
    ("l" "line" (lambda () (interactive) (org-table-recalculate nil)))]
   ["Source"
    :if org-in-src-block-p
    ("e" "run block" org-babel-execute-src-block)
    ("c" "check headers" org-babel-check-src-block)]
   ["More"
    ("c" "update checkbox count" org-update-checkbox-count)]
   [("f" "format" org-table-align :if org-at-table-p)
    ("q" "quit" transient-quit-all)]])

(shk-org-menu-add-navigation-items 'shk-org-menu-eval)

(defun shk-org-menu-insert-block (str)
  "Insert an org mode block of type `str'"
  (interactive)
  (insert (format "#+begin_%s\n#+end_%s\n" str str)))

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
    ("bq" "quote" (lambda () (interactive) (shk-org-menu-insert-block "quote")))]
   ["Templates"
    ("S" "structure template" org-insert-structure-template)]
   [("q" "quit" transient-quit-all)]])

(shk-org-menu-add-navigation-items 'shk-org-menu-insert)

(transient-define-prefix shk-org-menu-table ()
  "Operations on org-mode tables"
  ["Tables"
   ["Navigate"
    ("p" "up" previous-line :transient t)
    ("n" "down" next-line :transient t)
    ("b" "left" org-table-previous-field :transient t)
    ("f" "right" org-table-next-field :transient t)]
   ["Move"
    ("P" "up" org-table-move-row-up :transient t)
    ("N" "down" org-table-move-row-down :transient t)
    ("B" "left" org-table-move-column-left :transient t)
    ("F" "right" org-table-move-column-right :transient t)]
   ["Create/delete"
    ("mr" "row above" org-table-insert-row :transient t)
    ("mc" "column right" org-table-insert-column :transient t)
    ("m-" "horiz. line" org-table-insert-hline :transient t)
    ("dr" "delete row" org-shiftmetaup :transient t)
    ("dc" "delete column" org-shiftmetaleft :transient t)
    ("mt" "table from region" org-table-create-or-convert-from-region)]
   ["Formulas"
    ("e" "edit all" org-table-edit-formulas :transient t)
    ("=" "field" (lambda () (interactive) (org-table-eval-formula '(4))) :transient t)
    ("+" "in place" (lambda () (interactive) (org-table-eval-formula '(16))))
    ("c" "column" org-table-eval-formula :transient t)
    ("h" "coordinates" org-table-toggle-coordinate-overlays :transient t)
    ("D" "debug" org-table-toggle-coordinate-overlays :transient t)]
   ["Field"
    ("E" "edit" org-table-edit-field :transient t)
    ("SPC" "blank" org-table-blank-field :transient t)
    ("RET" "from above" org-table-copy-down :transient t)]
   ["More"
    ("x" "evaluate" shk-org-menu-eval)
    ("S" "shrink column" org-table-toggle-column-width :transient t)
    ("q" "quit" transient-quit-all)]])

;; shk-org-menu-table does not have a headlines Navigation menu (too crowded)

(transient-define-prefix shk-org-menu-list ()
  "Operations on org-mode lists"
  ["List"
   ["Navigation"
    ("C-p" "prev" previous-line :transient t)
    ("C-n" "next" next-line :transient t)
    ("c" "cycle" org-cycle :transient t)
    ("u" "parent" org-up-element :transient t)
    ("p" "prev (same level)" org-backward-element :transient t)
    ("n" "next (same level)" org-forward-element :transient t)]
   ["Move"
    ("P" "up" org-metaup :transient t)
    ("N" "down" org-metadown :transient t)
    ("B" "left" org-shiftmetaleft :transient t)
    ("F" "right" org-shiftmetaright :transient t)
    ("b" "left (line)" org-metaleft :transient t)
    ("f" "right (line)" org-metaright :transient t)]
   ["List"
    ("R" "repair" org-list-repair)
    ("*" "turn into tree" org-list-make-subtree)]
   ["Toggle"
    ("-" "list item" org-toggle-item :if-not org-at-table-p :transient t)
    ("+" "list style" org-cycle-list-bullet :if-not org-at-table-p :transient t)]
   [("q" "quit" transient-quit-all)]])

;; Has a Navigation menu, already

(transient-define-prefix shk-org-menu ()
  "A discoverable menu to edit and view org-mode documents"
  ["Org mode"
   ["Go to"
    ("gs" "goto src block" org-babel-goto-named-src-block)
    ("gr" "goto result block" org-babel-goto-named-result)
    ("gh" "goto heading" imenu)]
   ["Elements"
    ("t" "table" shk-org-menu-table)
    ("l" "list" shk-org-menu-list)]
   ["Tasks"
    ("s" "structure" shk-org-menu-structure)
    ("v" "visibility" shk-org-menu-visibility)
    ("x" "evaluation" shk-org-menu-eval)
    ("i" "insert" shk-org-menu-insert)
    ("q" "quit" transient-quit-all)]])

(shk-org-menu-add-navigation-items 'shk-org-menu)
