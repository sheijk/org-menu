;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transient interface for org

(defvar shk-org-menu-navigation-items
  '(("p" "prev" org-previous-visible-heading :transient t)
    ("n" "next" org-next-visible-heading :transient t)
    ("c" "cycle" org-cycle :transient t)
    ("u" "up" outline-up-heading :transient t)
    ("P" "prev (same level)" shk-org-prev-heading :transient t)
    ("N" "next (same level)" shk-org-next-heading :transient t))
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
  [:description
   "Edit document structure"
   ["Move"
    ("P" "move prev" org-metaup :transient t)
    ("N" "move next" org-metadown :transient t)
    ("F" "indent tree" org-shiftmetaright :transient t)
    ("B" "unindent tree" org-shiftmetaleft :transient t)
    ("f" "indent line" org-metaright :transient t)
    ("b" "unindent line" org-metaleft :transient t)]
   ["Edit"
    ("R" "refile" org-refile)
    ("C-w" "cut tree" org-cut-special)
    ("C-y" "yank tree" org-paste-special)]
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
    ("*" "headline" org-ctrl-c-star :if-not org-at-table-p)]
   [("q" "quit" transient-quit-all)]])

(shk-org-menu-add-navigation-items 'shk-org-menu-structure)

(transient-define-prefix shk-org-menu-visibility ()
  "A menu to control visibility of org-mode items"
  [:description
   "Visibility"
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
  [:description
   "Evaluation"
   ["Table"
    :if org-at-table-p
    ("e" "table" (lambda () (interactive) (org-table-recalculate 'iterate)))
    ("1" "one iteration" (lambda () (interactive) (org-table-recalculate t)))
    ("l" "line" (lambda () (interactive) (org-table-recalculate nil)))]
   ["Source"
    :if org-in-src-block-p
    ("e" "run block" org-babel-execute-src-block)
    ("c" "check headers" org-babel-check-src-block)]
   [("f" "format" org-table-align :if org-at-table-p)
    ("q" "quit" transient-quit-all)]])

(shk-org-menu-add-navigation-items 'shk-org-menu-eval)

(transient-define-prefix shk-org-menu-insert ()
  "A menu to insert new items in org-mode"
  [:description
   "Insert"
   ["Time"
    ("." "time stamp" org-time-stamp)
    ("!" "inactive" org-time-stamp-inactive)
    ("t." "now" (lambda () (interactive) (org-insert-time-stamp (current-time) t)))
    ("t!" "now (inactive)" (lambda () (interactive) (org-insert-time-stamp (current-time) t)))]
   [("q" "quit" transient-quit-all)]])

(shk-org-menu-add-navigation-items 'shk-org-menu-insert)

(transient-define-prefix shk-org-menu ()
  "A discoverable menu to edit and view org-mode documents"
  [:description
   "Org mode"
   ["Go to"
    ("gs" "goto src block" org-babel-goto-named-src-block)
    ("gr" "goto result block" org-babel-goto-named-result)
    ("gh" "goto heading" imenu)]
   ["More"
    ("s" "structure" shk-org-menu-structure)
    ("v" "visibility" shk-org-menu-visibility)
    ("e" "evaluation" shk-org-menu-eval)
    ("i" "insert" shk-org-menu-insert)
    ("q" "quit" transient-quit-all)]])

(shk-org-menu-add-navigation-items 'shk-org-menu)
