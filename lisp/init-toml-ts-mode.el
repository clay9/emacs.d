;;; init-toml-ts-mode.el --- Toml Tree-sitter mode setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------
;;; 自动识别 Toml 文件
;; ------------------------------------------------------------
(add-to-list 'auto-mode-alist
             '("\\.toml\\'" . toml-ts-mode))

;; ------------------------------------------------------------
;;; Tree-sitter 安装与配置
;; ------------------------------------------------------------
(require 'fun-treesit)

(defun treesit/setup-toml ()
  "确保 Tree-sitter 语法可用。"
  (treesit/setup-language 'toml "https://github.com/tree-sitter/tree-sitter-toml"))

;; 加载 Tree-sitter
(unless (treesit-ready-p 'toml)
  (treesit/setup-toml))

(provide 'init-toml-ts-mode)
;;; init-toml-ts-mode.el ends here
