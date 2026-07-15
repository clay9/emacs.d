;;; init-compile.el --- Settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package compile
  :commands compile
  :bind (:map compilation-mode-map
              ("n" . compilation-next-error)
              ("p" . compilation-previous-error)
              ("TAB" . compilation-display-error)
              ("RET" . compile-goto-error))
  :config
  ;; 每次执行 compile 都提示输入命令
  (setq compilation-read-command t))

(provide 'init-compile)
;;; init-compile.el ends here
