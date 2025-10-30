;;; init-dockerfile-ts-mode.el --- Dockerfile Tree-sitter mode setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------
;;; 自动识别 Dockerfile 文件
;; ------------------------------------------------------------
(add-to-list 'auto-mode-alist
             '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
               . dockerfile-ts-mode))

;; ------------------------------------------------------------
;;; Tree-sitter 安装与配置
;; ------------------------------------------------------------
(require 'fun-treesit)

(treesit/load 'dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")

(provide 'init-dockerfile-ts-mode)
;;; init-dockerfile-ts-mode.el ends here
