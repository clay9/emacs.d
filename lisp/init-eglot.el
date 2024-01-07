;;; init-eglot.el --- LSP support via eglot   -*- lexical-binding: t; -*-
;;; Commentary:

(use-package eglot
  :commands (eglot)
  :init
  ;; (with-eval-after-load 'c-ts-mode
  ;;   (add-hook 'c++-ts-mode-hook 'eglot-ensure))
  :config
  (setq read-process-output-max (* 1024 1024)
        eglot-withhold-process-id t)
  (add-to-list 'eglot-server-programs '(c++-ts-mode . ("clangd")))
  (advice-add 'jsonrpc--log-event :override #'ignore)
  (setopt eglot-events-buffer-size 0))

(provide 'init-eglot)
;;; init-eglot.el ends here
