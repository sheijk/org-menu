;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transient interface for org

(define-transient-command shk-org-structure-menu
  "Edit structure of org file"
  [:description
   "Edit document structure"
   ["Navigate"
    ("p" "prev" org-previous-visible-heading :transient t)
    ("n" "next" org-next-visible-heading :transient t)
    ("c" "cycle" org-cycle :transient t)
    ("u" "up" outline-up-heading :transient t)]
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

(defun shk-org-tree-show-all ()
  (interactive)
  (org-show-set-visibility 'tree))

(defun shk-org-set-startup-visibility ()
  "Will set visibility like it is after loading the org document."
  (interactive)
  (org-set-startup-visibility))

(defun shk-org-reveal-full ()
  "Will make current tree and all parent and sibling nodes visible."
  (interactive)
  (org-reveal t))

(define-transient-command shk-org-visibility-menu
  "Visibility"
  [:description
   "Visibility"
   ["Navigate"
    ("p" "prev" org-previous-visible-heading :transient t)
    ("n" "next" org-next-visible-heading :transient t)
    ("u" "up" outline-up-heading :transient t)
    ("P" "prev (same level)" shk-org-prev-heading :transient t)
    ("N" "next (same level)" shk-org-next-heading :transient t)]
   ["Tree"
    ("c" "cycle" org-cycle :transient t)
    ("a" "all" org-show-subtree :if-not org-at-block-p :transient t)
    ("a" "all" org-hide-block-toggle :if org-at-block-p :transient t)
    ("t" "content" shk-org-show-content :if-not org-at-block-p :transient t)
    ("h" "hide" outline-hide-subtree :if-not org-at-block-p :transient t)
    ("h" "hide" org-hide-block-toggle :if org-at-block-p :transient t)
    ("r" "reveal" shk-org-reveal-full :if-not org-at-block-p :transient t)
    ]
   ["Global"
    ("C" "cycle global" org-global-cycle :transient t)
    ("go" "overview" org-overview)
    ("gc" "content" org-content)
    ("ga" "all" org-show-all)
    ("gd" "all" shk-org-set-startup-visibility)]
   [("q" "quit" transient-quit-all)]])

(define-transient-command shk-org-eval-menu
  "Evaluation"
  [:description
   "Evaluation"
   [("e" "table" (lambda () (interactive) (org-table-recalculate 'iterate)) :if org-at-table-p)
    ("1" "one iteration" (lambda () (interactive) (org-table-recalculate t)) :if org-at-table-p)
    ("l" "line" (lambda () (interactive) (org-table-recalculate nil)) :if org-at-table-p)]
   [("f" "format" org-table-align :if org-at-table-p)
    ("q" "quit" transient-quit-all)]])

(define-transient-command shk-org-menu
  "Org mode"
  [:description
   "Org mode"
   ["Navigate"
    ("p" "prev" org-previous-visible-heading :transient t)
    ("n" "next" org-next-visible-heading :transient t)
    ("u" "up" outline-up-heading :transient t)
    ("c" "cycle folding" org-cycle :transient t)
    ("P" "prev (same level)" shk-org-prev-heading :transient t)
    ("N" "next (same level)" shk-org-next-heading :transient t)]
   [("s" "structure" shk-org-structure-menu)
    ("v" "visibility" shk-org-visibility-menu)
    ("e" "evaluation" shk-org-eval-menu)
    ("q" "quit" transient-quit-all)]])
