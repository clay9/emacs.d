;;; init-text-show.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq-default
 truncate-lines nil
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'transient-mark-mode)
(add-hook 'after-init-hook 'electric-pair-mode)
(add-hook 'after-init-hook 'show-paren-mode)

(add-hook 'after-init-hook 'delete-selection-mode)
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'after-init-hook 'global-eldoc-mode)
(diminish 'eldoc-mode)


;; show whitespace
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; show line-number
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))


;; show page break lines (^L => page break line: ----)
(use-package page-break-lines
  :diminish page-break-lines-mode
  :hook (after-init . global-page-break-lines-mode))


;; show column indicator
;; (when (boundp 'display-fill-column-indicator)
;;   (setq-default indicate-buffer-boundaries 'left)
;;   (setq-default display-fill-column-indicator-character ?\u254e)
;;   (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))

(provide 'init-text-show)
;;; init-text-show.el ends here
