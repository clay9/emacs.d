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
;; TODOLATER 后期删除
;; 现在保留的原因是 package ‘rg’ 依赖 ‘wgrep’
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
;; 3. 使用grep-edit-mode(hotkey: e)编辑grep-like buffer

;; 上面的replace流程为了生成grep-like buffer有点复杂. 直接使用rg.el生成
(defun text/rg-project ()
  (interactive)
  (call-interactively 'rg-project)
  (let ((win (get-buffer-window "*rg*")))
    (when win
      (select-window win))))


(provide 'init-text-grep)
;;; init-text-grep.el ends here
