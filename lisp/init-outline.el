;;; init-outline.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package symbols-outline
  :commands (symbols-outline-show)
  :config
  (setq symbols-outline-window-position 'left
        symbols-outline-no-other-window nil
        symbols-outline-fetch-fn #'symbols-outline-lsp-fetch)
  (symbols-outline-follow-mode))

(provide 'init-outline)
;;; init-outline.el ends here
