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

(treesit/load 'toml "https://github.com/tree-sitter/tree-sitter-toml")

(provide 'init-toml-ts-mode)
;;; init-toml-ts-mode.el ends here
