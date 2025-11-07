;;; init-emacs-lisp-mode.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -----------------------------
;;; outline + consult-outline 设置
;; -----------------------------
(defun emacs-lisp/setup-init-outline ()
  "Enable outline-minor-mode for init.el style files and set local outline-regexp."
  (outline-minor-mode 1)
  ;; 识别一级/二级标题
  (setq-local outline-regexp "^ *\\(;;;+\\) ")
  ;; 默认折叠，只显示二级标题
  ;;(outline-hide-sublevels 2)
  )

;; 在 elisp buffer 打开时启用
(add-hook 'emacs-lisp-mode-hook #'emacs-lisp/setup-init-outline)
(add-hook 'lisp-interaction-mode-hook #'emacs-lisp/setup-init-outline)

;; 每次保存Emacs lisp文件前自动缩进
(add-hook 'before-save-hook
          (lambda()
            (when (eq major-mode 'emacs-lisp-mode)
              (indent-region (point-min) (point-max)))))

;; -----------------------------
;;; 快捷键
;; -----------------------------
(define-key emacs-lisp-mode-map (kbd "C-x C-e") 'eval-defun)
(define-key lisp-interaction-mode-map (kbd "C-x C-e") 'eval-defun)

(provide 'init-emacs-lisp-mode)
;;; init-emacs-lisp-mode.el ends here
