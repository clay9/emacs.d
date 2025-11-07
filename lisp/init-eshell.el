;;; init-eshell.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eshell
  :config
  ;; 设置 eshell 的存储目录
  (setq eshell-directory-name (concat my/ecfg-dir "eshell/"))

  ;; 取消默认 clear-scrollback 函数绑定
  (fmakunbound 'eshell/clear-scrollback)

  ;; 自定义清屏函数
  (defun eshell/clear ()
    "Override eshell/clear-scrollback: 清空 buffer 内容。"
    (let ((inhibit-read-only t))
      (erase-buffer))))

(provide 'init-eshell)
;;; init-eshell.el ends here
