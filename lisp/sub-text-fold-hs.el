;;; sub-text-fold-hs.el --- Text folding: hs-mode -*- lexical-binding: t -*-
;;; Commentary:
;; This file provides commands and keybindings for folding/hiding text:
;;   - hideshow minor mode for text
;;; Code:

(use-package hideshow
  :ensure nil
  :config
  ;; Hide minor mode indicator in the mode line.
  (when-let* ((cell (assq 'hs-minor-mode minor-mode-alist)))
    (setcdr cell '("")))

  ;; =====================
  ;;; hideshow minor cycle
  ;; =====================
  (defun text/hs-cycle ()
    "Org-style cycle for outline mode."
    (cond
     ;; 1. 不在entry中. toggle all
     ((save-excursion
        (not (or (hs-find-block-beginning)
                 (hs-inside-comment-p)
                 )))
      (let ((any-hidden
             (cl-some (lambda (ov)
                        (and (overlay-get ov 'hs)
                             (overlay-get ov 'invisible)))
                      (overlays-in (point-min) (point-max)))))
        (if any-hidden
            ;; 1.1 有任何折叠的node, 则展开所有
            (hs-show-all)
          ;; 1.2 所有node都已展开, 则关闭所有
          (hs-hide-all))))
     ;; 2. 在entry中
     (t (hs-cycle)))))

(provide 'sub-text-fold-hs)
;;; sub-text-fold-hs.el ends here
