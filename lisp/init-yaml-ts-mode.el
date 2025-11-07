;;; init-yaml-ts-mode.el --- YAML Tree-sitter mode setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------
;;; 自动识别 YAML 文件
;; ------------------------------------------------------------
(add-to-list 'auto-mode-alist
             '("\\.ya?ml\\'" . yaml-ts-mode))

;; ------------------------------------------------------------
;;; Tree-sitter 安装与配置
;; ------------------------------------------------------------
(require 'fun-treesit)

(treesit/load 'yaml "https://github.com/ikatyang/tree-sitter-yaml")

(provide 'init-yaml-ts-mode)
;;; init-yaml-ts-mode.el ends here
