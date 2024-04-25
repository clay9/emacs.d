;;; init-yaml-mode.el --- Support Yaml files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

(provide 'init-yaml-mode)
;;; init-yaml-mode.el ends here
