;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transient interface for org

(define-transient-command shk-org-structure-menu
  "Edit structure of org file"
  [:description
   "Edit document structure"
   ["Navigate"
    ("p" "prev" org-previous-visible-heading :transient t)
    ("n" "next" org-next-visible-heading :transient t)]
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
   ["Tree"
    ("c" "cycle" org-cycle :transient t)
    ("a" "all" shk-org-tree-show-all)
    ("t" "hide" org-hide-block-toggle :if org-at-block-p)
    ("R" "reveal" shk-org-reveal-full)
    ]
   ["Global"
    ("C" "cycle global" org-global-cycle :transient t)
    ("go" "overview" org-overview)
    ("gc" "content" org-content)
    ("ga" "all" org-show-all)
    ("gd" "all" shk-org-set-startup-visibility)]
   [("q" "quit" transient-quit-all)]])

(define-transient-command shk-org-menu
  "Org mode"
  [:description
   "Org mode"
   ["Navigate"
    ("p" "prev" org-previous-visible-heading :transient t)
    ("n" "next" org-next-visible-heading :transient t)
    ("u" "up" outline-up-heading :transient t)
    ("P" "prev (same level)" shk-org-prev-heading :transient t)
    ("N" "next (same level)" shk-org-next-heading :transient t)]
   [("e" "edit structure" shk-org-structure-menu)
    ("v" "visibility" shk-org-visibility-menu)
    ("q" "quit" transient-quit-all)]])

