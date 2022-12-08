(require-package 'smartparens)
(require 'smartparens-config)

;; hooks
(add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
(add-hook 'c-mode-common-hook  'smartparens-mode)
(add-hook 'org-mode-hook 'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)

;; open mode
(show-smartparens-global-mode t)

;; set pairs for org-mode
(sp-with-modes 'org-mode
  (sp-local-pair "=" "=" :wrap "C-="))
(sp-with-modes 'org-mode
  (sp-local-pair ":" ":" :wrap "C-="))


;; set pairs for C++
(sp-with-modes '(malabar-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
(sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
						    ("* ||\n[i]" "RET")))

;; set pairs for lisp-mode
(sp-with-modes sp-lisp-modes
  (sp-local-pair "(" nil
		 :wrap "C-("
		 :pre-handlers '(my-add-space-before-sexp-insertion)
		 :post-handlers '(my-add-space-after-sexp-insertion)))


(provide 'init-smartparens-mode)
