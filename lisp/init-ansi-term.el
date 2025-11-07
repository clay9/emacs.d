;;; init-ansi-term.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package term
  :commands ansi-term
  :bind
  (:map term-raw-map
        ("C-j" . term-line-mode)
   :map term-mode-map
        ("C-j" . term-char-mode))
  :config
  ;; 退出时自动杀掉 buffer
  (defun ansi-term/handle-exit (&optional process-name msg)
    "Kill the current term buffer when the terminal process exits."
    (kill-buffer (current-buffer)))

  ;; 在 term-handle-exit 之后调用自定义函数
  (advice-add 'term-handle-exit :after #'ansi-term/handle-exit))

(provide 'init-ansi-term)
;;; init-ansi-term.el ends here
