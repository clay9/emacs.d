;;; init-text-snippet.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :config
  (when-let* ((cell (assq 'yas-minor-mode minor-mode-alist)))
    (setcdr cell '("")))
  (setq yas-snippet-dirs (list (expand-file-name ".yasnippet" user-emacs-directory)))
  (yas-global-mode t))

(provide 'init-text-snippet)
;;; init-text-snippet.el ends here
