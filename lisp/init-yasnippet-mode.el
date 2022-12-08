(require-package 'yasnippet)
(require 'yasnippet)

(setq yas-snippet-dirs (list (concat my/ecfg-dir "yasnippet")))

;; start must after settings, or settings will not work
(yas-global-mode)

(provide 'init-yasnippet-mode)
