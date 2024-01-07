;;; init-text-snippet.el --- Settings for snippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (list (expand-file-name ".yasnippet" user-emacs-directory)))
  ;; start must after settings, or settings will not work
  (yas-global-mode t))

(provide 'init-text-snippet)
;;; init-text-snippet.el ends here
