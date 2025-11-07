;;; fun-treesit.el --- 公共 Tree-sitter 安装函数 -*- lexical-binding: t -*-
;;; Commentary:
;;; 提供一个通用函数 treesit/setup-language 安装 Tree-sitter 语法
;;; Code:

(require 'treesit)

;; ------------------------------------------------------------
;;; 公共 Tree-sitter 安装函数
;; ------------------------------------------------------------
(defun treesit/setup-language (lang repo)
  "确保 LANG 对应的 Tree-sitter 语法可用，如果缺失则安装。
LANG 是语言符号，例如 'cpp、'c、'dockerfile。
REPO 是对应的 tree-sitter 仓库 URL。"
  (add-to-list 'treesit-language-source-alist (list lang repo))
  (unless (treesit-language-available-p lang)
    (treesit-install-language-grammar lang)))

(defun treesit/load (lang repo)
  (unless (treesit-ready-p lang)
    (treesit/setup-language lang repo)))


(provide 'fun-treesit)
;;; init-treesit.el ends here
