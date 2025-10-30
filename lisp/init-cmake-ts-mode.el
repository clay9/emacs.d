;;; init-cmake-ts-mode.el --- Cmake ts mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------
;;; 自动识别
;; ------------------------------------------------------------
(add-to-list 'auto-mode-alist
             '("\\(CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode))

;; ------------------------------------------------------------
;;; Tree-sitter 安装与配置
;; ------------------------------------------------------------
(require 'fun-treesit)

(treesit/load 'cmake "https://github.com/uyha/tree-sitter-cmake")

(provide 'init-cmake-ts-mode)
;;; init-cmake-ts-mode.el ends here
