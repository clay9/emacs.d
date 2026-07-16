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
    (let* ((base-rules (cdr (car (c-ts-mode--simple-indent-rules 'cpp 'k&r))))
           (google-rules `(;; namespace
                           ((n-p-gp nil nil "namespace_definition") grand-parent 0)
                           ;; class 列表初始化中的 `:'
                           ((node-is "field_initializer_list") parent-bol 4)
                           ;; class `public', `private'
                           ((node-is "access_specifier") parent-bol 1)
                           ;; class 中的函数|变量 声明
                           ((and (node-is "field_declaration")(parent-is "field_declaration_list")) parent-bol c-ts-mode-indent-offset)
                           ;; class 中的函数|变量 定义
                           ((and (node-is "function_definition")(parent-is "field_declaration_list")) parent-bol c-ts-mode-indent-offset)
                           ;; 处理长函数调用、参数、赋值换行
                           ;; 第一个命名的参数. 因为`('会被当作第一个参数, 所以这里添加命名过滤一下
                           ((and (parent-is "argument_list")
                                 (lambda (node &rest _)
                                   (null (treesit-node-prev-sibling node t)))) ; t 表示只寻找命名的兄弟节点
                            parent-bol 4)
                           ,@base-rules)))
      ;; fix bug：返回必须带有语言符号头，彻底根除 listp, parent-bol 错误
      `((cpp . ,google-rules)))))

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
