(require-package 'symbol-overlay)
(require 'symbol-overlay)

;;hook
(add-hook 'dired-mode-hook 'symbol-overlay-mode)
(add-hook 'eshell-mode-hook 'symbol-overlay-mode)
(add-hook 'c-mode-common-hook 'symbol-overlay-mode)
(add-hook 'asm-mode-hook 'symbol-overlay-mode)
(add-hook 'emacs-lisp-mode-hook 'symbol-overlay-mode)


(provide 'init-symbol-overlay)
