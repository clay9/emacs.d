;;; init-c++-ts-mode.el --- C/C++ Tree-sitter mode setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------
;;; Google Style 缩进
;; ------------------------------------------------------------
(with-eval-after-load 'c-ts-mode
  (defvar my/google-c-style-cache nil
    "缓存 Google C/C++ Tree-sitter 缩进规则。")

  (defun google-c-style ()
    "Override the built-in `gnu' indentation style with Google C++ rules."
    (or my/google-c-style-cache
        (setq my/google-c-style-cache
              `(((node-is ")") parent-bol 0)
                ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
                ((parent-is "argument_list") first-sibling 1)
                ((match "parameter_declaration" "parameter_list" nil nil nil) first-sibling 1)
                ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
                ((parent-is "parameter_list") prev-sibling 0)
                ((n-p-gp nil nil "namespace_definition") grand-parent 0)
                ((match "access_specifier" "base_class_clause" nil nil nil) parent-bol 0)
                ((match "access_specifier" "field_declaration_list" nil nil nil) parent-bol 1)
                ((node-is "field_initializer_list") parent-bol 4)
                ((match nil "field_initializer_list" nil 2 nil) parent-bol 2)
                ((node-is "case_statement") parent-bol 2)
                ((node-is "field_identifier") prev-sibling 0)
                ;; 继承 GNU 样式
                ,@(alist-get 'gnu (c-ts-mode--indent-styles 'cpp))))))

  (setq c-ts-mode-indent-style #'google-c-style))

;; ------------------------------------------------------------
;;; transient 快捷键
;; ------------------------------------------------------------
(with-eval-after-load 'c-ts-mode
  (transient-define-prefix transient/c++-ts-mode ()
    [:class transient-column "navigation"
            ("f" "go-forward" xref-go-forward)
            ("b" "go-back" xref-go-back)])
  (define-key c++-ts-mode-map (kbd "C-j") 'transient/c++-ts-mode))

;; ------------------------------------------------------------
;;; major-mode remap
;; ------------------------------------------------------------
(dolist (pair '((c-mode . c-ts-mode)
                (c++-mode . c++-ts-mode)
                (c-or-c++-mode . c-or-c++-ts-mode)))
  (add-to-list 'major-mode-remap-alist pair))

;; ------------------------------------------------------------
;;; Tree-sitter 安装与配置
;; ------------------------------------------------------------
(require 'fun-treesit)

;; (treesit/load 'c   "https://github.com/tree-sitter/tree-sitter-c")
(treesit/load 'cpp "https://github.com/tree-sitter/tree-sitter-cpp")

(provide 'init-c++-ts-mode)
;;; init-c++-ts-mode.el ends here
