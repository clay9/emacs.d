;;; init-c++-ts-mode.el --- C/C++ Tree-sitter mode setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ------------------------------------------------------------
;;; Google Style 缩进
;; ------------------------------------------------------------
;; 详细的ts indent信息
;; (setq treesit--indent-verbose t)

(use-package c-ts-mode
  :ensure nil
  :init
  (setq c-ts-mode-indent-style #'my-c-ts-google-rules-complete)

  :config
  (defun my-c-ts-google-rules-complete ()
    "Google C++ Style layout rules for c-ts-mode with explicit language tags."
    (let ((core-rules `(((n-p-gp nil nil "namespace_definition") grand-parent 0)
                        ((node-is "}") parent-bol 0)
                        ((node-is "access_specifier") parent-bol 1)
                        ((parent-is "field_declaration_list") parent-bol c-ts-indent-offset)
                        ((parent-is "initializer_list") parent-bol 4)
                        ((node-is "initializer_pair") parent-bol 4)
                        ((node-is "case_statement") parent-bol 0)
                        ((parent-is "case_statement") parent-bol c-ts-indent-offset)
                        ((parent-is "if_statement") parent-bol c-ts-indent-offset)
                        ((parent-is "for_statement") parent-bol c-ts-indent-offset)
                        ((parent-is "while_statement") parent-bol c-ts-indent-offset)
                        ((parent-is "do_statement") parent-bol c-ts-indent-offset)
                        ((parent-is "try_statement") parent-bol c-ts-indent-offset)
                        ((parent-is "catch_clause") parent-bol c-ts-indent-offset)
                        ((parent-is "translation_unit") parent-bol 0)
                        ((parent-is "compound_statement") parent-bol c-ts-indent-offset)
                        ((node-is ")") parent-bol 0)
                        ((parent-is "parameter_list") parent-bol c-ts-indent-offset)
                        ((parent-is "argument_list") parent-bol c-ts-indent-offset)
                        (no-node parent-bol 0)
                        (,(lambda (_ _ _) t) parent-bol 0))))
      ;; fix bug：返回必须带有语言符号头，彻底根除 listp, parent-bol 错误
      `((c . ,core-rules)
        (cpp . ,core-rules)))))

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
