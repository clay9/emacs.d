;;; init-eshell.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package eshell
  ;; :bind ( :map eshell-prompt-mode-map
  ;;         ("C-c C-p" . eshell-previous-prompt))
  :config
  (setq eshell-directory-name (concat my/ecfg-dir "eshell/")))

(provide 'init-eshell)
;;; init-eshell.el ends here
