;;; init-vertico-suite.el --- Minibuffer completion system -*- lexical-binding: t -*-
;;; Commentary:
;; 基于 Vertico 的 minibuffer 补全生态：
;; - vertico: minibuffer completion UI
;; - orderless: 高级匹配风格
;; - consult: 增强的命令和候选
;; - marginalia: 显示候选补充信息
;; - embark: 对候选执行操作
;;; Code:

;; ------------------------------------------------------------
;;; Vertico: minibuffer completion UI
;; ------------------------------------------------------------
(use-package vertico
  :hook (after-init . vertico-mode))

;; ------------------------------------------------------------
;;; Orderless: 高级匹配风格
;; ------------------------------------------------------------
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ------------------------------------------------------------
;;; Marginalia: minibuffer 注释信息
;; ------------------------------------------------------------
(use-package marginalia
  :hook (after-init . marginalia-mode))

;; ------------------------------------------------------------
;;; Consult: minibuffer 命令与候选增强
;; ------------------------------------------------------------
(use-package consult
  :config
  ;; 设置consult buffer过滤规则
  (setq consult-buffer-filter
        '("\\` "                  ;; 空格开头的缓冲区
          "\\`\\*[^s][^c]*\\*"    ;; 隐藏 * 开头但不是 *scratch*
          "\\.jd\\'"              ;; 隐藏.jd 日记文件
          "inbox.org" "task.org" "archive.org"  ;; 隐藏gtd buffers
          "emacs.org" "qygame.org" )))

;; ------------------------------------------------------------
;;; Embark: actions for minibuffer/buffer candidates
;; ------------------------------------------------------------
(use-package embark
  :config
  ;; 清空并重建 identifier keymap
  (setcdr embark-identifier-map nil)
  (define-keymap
    :keymap embark-identifier-map
    ;; 导航相关
    "n" #'embark-next-symbol
    "p" #'embark-previous-symbol
    "d" #'xref-find-definitions
    "r" #'xref-find-references
    ;; 编辑相关
    "R" (lambda ()
          (interactive)
          (call-interactively 'eglot-rename))
    "t" #'replace-string
    ";" #'comment-dwim
    ;; 搜索 / 高亮
    "s" #'consult-line
    "h" #'embark-toggle-highlight
    ;; 复制
    "w" #'embark-copy-as-kill))

;; 与 consult 集成
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-vertico-suite)
;;; init-vertico-suite.el ends here
