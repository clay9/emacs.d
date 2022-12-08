(require-package 'flycheck)
(require 'flycheck)

;(add-hook 'c-mode-common-hook 'flycheck-mode)

;; c++ mode时候默认使用c++11标准
;(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)


(provide 'init-flycheck-mode)
