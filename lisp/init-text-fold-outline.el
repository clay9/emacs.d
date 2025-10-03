;;; init-text-fold-outline.el --- Text folding: outline mode -*- lexical-binding: t -*-
;;; Commentary:
;;; This file provides commands and keybindings for folding/hiding text:
;;;   - Outline minor mode toggles
;;; Code:

;; ----------------------
;;; Outline minor mode toggles
;; ----------------------
(defun text/outline-toggle-all ()
  "Toggle visibility of outline sublevels.
If any sublevel is hidden, show all.
If all sublevels are visible, hide to only top-level headings."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (catch 'hidden-found
          (while (not (eobp))
            (when (outline-invisible-p (point))
              (throw 'hidden-found t))
            (forward-line 1))
          nil)
        (outline-show-all)
      (outline-hide-sublevels 1))))

(defun text/outline-cycle ()
  "Org-style cycle for outline minor mode.
- If entry is folded, show its body and direct children.
- If subtree is fully folded, expand the whole subtree.
- Otherwise, fold the subtree."
  (interactive)
  (outline-back-to-heading)
  (cond
   ;; 1. 当前 entry 折叠 → 展开 entry body 和 children
   ((outline-invisible-p (line-end-position))
    (outline-show-entry)
    (outline-show-children))

   ;; 2. 如果 subtree 完全折叠 → 展开整个 subtree
   ((save-excursion
      (let ((end (save-excursion
                   (outline-end-of-subtree)
                   (outline-back-to-heading)
                   (point)))
            (all-folded t))
        (while (and all-folded (< (point) end))
          (outline-next-heading)
          ;; 判断是否有body && 是否折叠
          (let ((has_body (/= (line-end-position)
                              (save-excursion (outline-end-of-subtree) (point))))
                (not_folded (not (outline-invisible-p (line-end-position)))))
            (when (and has_body not_folded)
              (setq all-folded nil))))
        all-folded))
    (outline-show-subtree))

   ;; 3. 默认(有body展开, 包含entry body) → 折叠entry
   (t
    (outline-hide-subtree))))

(transient-define-prefix transient/outline-minor-mode ()
  "Common outline commands."
  [[:class transient-column "hide"
           ("TAB" "toggle" text/outline-cycle)
           ("a" "toggle all" text/outline-toggle-all)
           ("o" "hide other" outline-hide-other)]
   [:class transient-column "navigate"
           ("p" "previous visible heading" outline-previous-visible-heading)
           ("n" "next visible heading" outline-next-visible-heading)
           ("f" "forward same heading" outline-forward-same-level)
           ("b" "backward same heading" outline-backward-same-level)
           ("u" "up heading" outline-up-heading)]])

(provide 'init-text-fold-outline)
;;; init-text-fold-outline.el ends here
