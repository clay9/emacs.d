;;; init-text-grep.el --- Grep -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; =====================
;;; rg
;; =====================
(use-package rg
  :ensure t
  :commands (rg rg-literal rg-project))

;; =====================
;;; wgrep
;; =====================
(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode)


;; =====================
;;; search && replace
;; =====================
;; search: 'consult-line', 'consult-ripgrep'
;; replace 流程
;; 1. consult-ripgrep 匹配候选项
;; 2. embark-export 输出minibuffer候选到普通buffer中
;; 3. 使用wgrep(C-c C-p)编辑grep-like buffer

;; 上面的replace流程为了生成grep-like buffer有点复杂. 直接使用rg.el生成
(defun my/rg-project ()
  (interactive)
  (call-interactively 'rg-project)
  (let ((win (get-buffer-window "*rg*")))
    (when win
      (select-window win))))


(provide 'init-text-grep)
;;; init-text-grep.el ends here
