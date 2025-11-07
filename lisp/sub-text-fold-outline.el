;;; sub-text-fold-outline.el --- Text folding: outline mode -*- lexical-binding: t -*-
;;; Commentary:
;; This file provides commands and keybindings for folding/hiding text:
;;   - Outline minor mode toggles
;;; Code:

(use-package outline
  :ensure nil
  :config
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
    "Org-style cycle for outline mode.
- If entry is folded, show its body and subtree(folded).
- If entry has subtree and subtree is fully folded, expand the whole subtree.
- Otherwise, fold the entry."
    (interactive)
    (cond
     ;; 1. 当前 entry 折叠 → 展开 entry body 和 children(folded)
     ((save-excursion
        (outline-back-to-heading)
        (outline-invisible-p (line-end-position)))
      (outline-show-entry)
      (outline-show-children))

     ;; 2. 如果有subtree 且 subtree完全折叠 → 展开整个 subtree
     ((let* ((beg (save-excursion
                    (outline-back-to-heading)
                    (point)))
             (end (save-excursion
                    (outline-end-of-subtree)
                    (outline-back-to-heading)
                    (point)))
             ;; has subtree and all subtree folded
             ;; beg = end: 没有subtree. set all-folded nil
             (all-folded (not (= beg end))))
        (save-excursion
          (outline-back-to-heading)
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

     ;; 3. 其他情况 → 折叠entry
     (t
      (outline-hide-subtree)))))

(provide 'sub-text-fold-outline)
;;; sub-text-fold-outline.el ends here
