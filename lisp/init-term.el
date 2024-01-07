;;; init-term.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package term
  :commands ansi-term
  :bind ( :map term-raw-map
          ("C-j" . term-line-mode)
          :map term-mode-map
          ("C-j" . term-char-mode))
  :config
  (defun my/term-handle-exit (&optional process-name msg)
    (kill-buffer (current-buffer)))
  (advice-add 'term-handle-exit :after 'my/term-handle-exit))

(provide 'init-term)
;;; init-term.el ends here
