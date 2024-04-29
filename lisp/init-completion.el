;;; init-completion.el --- completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; completion 通用配置
(setq tab-always-indent 'complete)


;;; complete -- 基于emacs completion, 提供候选 provide candidates
(use-package consult
  :config
  ;; buffer filter
  (add-to-list 'consult-buffer-filter ".DS_Store")
  (add-to-list 'consult-buffer-filter "\\`\\*Messages\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Ibuffer\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Org Agenda\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Magit Repositories\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Calendar\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Disabled Command\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Backtrace\\*\\'")
  (add-to-list 'consult-buffer-filter "\\*EGLOT*")
  (add-to-list 'consult-buffer-filter "magit-process:*")

  (add-to-list 'consult-buffer-filter "diary.org")
  (add-to-list 'consult-buffer-filter "life.org")
  (add-to-list 'consult-buffer-filter "inbox.org")
  (add-to-list 'consult-buffer-filter "task.org")
  (add-to-list 'consult-buffer-filter "archive.org")
  (add-to-list 'consult-buffer-filter "emacs.org")
  (add-to-list 'consult-buffer-filter "qygame.org")
  (add-to-list 'consult-buffer-filter "algo.org"))


;;; 为候选添加注释信息
(use-package marginalia
  :hook (after-init . marginalia-mode))


;;; 候选过滤匹配规则
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;; minibuffer complete-ui
(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  ;; change vertico sort
  (setq vertico-sort-function 'vertico-sort-alpha))


;;; buffer complete-ui (instead of company-mode)
(use-package corfu
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))


;;; actions for minibuffer && buffer
(use-package embark)

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

(provide 'init-completion)
;;; init-completion.el ends here
