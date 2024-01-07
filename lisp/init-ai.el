;;; init-ai.el --- AI assistant via copilot   -*- lexical-binding: t; -*-
;;; Commentary:

;; TODOLATER: 也许某天AI Assistant成熟的时候, 可以再尝试
;;

;;; Code:


;;; copilot

;; 优势: 质量高, 速度快, 设置代理方便
;; 缺点: 贵

;; (use-package editorconfig)
;; (use-package dash)
;; (use-package s)
;; (use-package copilot
;;   :load-path "site-lisp/copilot"
;;   :requires (editorconfig dash s)
;;   :diminish
;;   :commands (copilot-login)
;;   :bind ( :map copilot-completion-map
;;           ("C-<return>" . copilot-accept-completion)
;;           ("M-p" . copilot-previous-completion)
;;           ("M-n" . copilot-next-completion))
;;   :config
;;   (setq copilot-network-proxy '(:host "127.0.0.1" :port "10887")))


;;; codeium

;; 优势: 便宜
;; 缺点: 质量一般, 速度慢(怀疑速度慢是代理的原因)
;;       速度慢引发了emacs卡顿, 因此不自动启用
;;       代理设置麻烦, 需要在vpn中特别指明codeium.com访问

(use-package codeium
  :load-path "site-lisp/codeium"
  :commands (codeium-init)
  :init
  ;; 不自动开启
  ;; (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  :config
  (setq use-dialog-box nil) ;; do not use popup boxes
  
  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion)))))


(provide 'init-ai)
;;; init-ai.el ends here
