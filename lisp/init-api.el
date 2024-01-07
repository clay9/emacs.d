;;; init-api.el --- api document -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package devdocs
  :config
  (setq devdocs-data-dir (concat my/ecfg-dir "devdocs"))
  (setq devdocs-window-select t)

  ;; resuse-window, or use the selected window
  (add-to-list 'display-buffer-alist
	       `("\\*devdocs"
                 (display-buffer-reuse-window display-buffer-same-window)))

  ;; install docks
  (unless (file-exists-p (concat devdocs-data-dir "/cpp"))
    (devdocs-install "cpp")))

(provide 'init-api)
;;; init-api.el ends here
