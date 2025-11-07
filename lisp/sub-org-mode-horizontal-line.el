;;; sub-org-mode-horizontal-line.el --- Horizontal Line -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/org-add-horizontal-lines ()
  "在 Org mode 中把 '-----' 显示为水平线。"
  (font-lock-add-keywords
   nil
   '(("^[ \t\*]*-----+$"
      (0 (ignore
          (let* ((ov (make-overlay (match-beginning 0) (match-end 0)))
                 (width (window-body-width))
                 (line (make-string width ?─)))
            (overlay-put ov 'display line)
            (overlay-put ov 'face '(:foreground "#888888"))
            (overlay-put ov 'my-org-hr t))))))))

(defun my/org-remove-horizontal-lines ()
  "清除 Org buffer 中的水平线 overlay。"
  (remove-overlays (point-min) (point-max) 'my-org-hr t))

(defun my/org-refresh-horizontal-lines ()
  "刷新 Org buffer 中的水平线 (适应窗口宽度变化)。"
  (interactive)
  (save-excursion
    (my/org-remove-horizontal-lines)
    (my/org-add-horizontal-lines)
    (font-lock-flush)))

(add-hook 'org-mode-hook #'my/org-add-horizontal-lines)
(add-hook 'window-size-change-functions
          (lambda (_frame)
            (my/org-refresh-horizontal-lines)))


(provide 'sub-org-mode-horizontal-line)
;;; sub-org-mode-horizontal-line.el ends here
