(require-package 'helm-gtags)
(require 'helm-gtags)

;; hooks
(add-hook 'c-mode-common-hook 'helm-gtags-mode)

;; helm-gtags
(setq
 helm-gtags-ignore-case nil
 helm-gtags-auto-update nil ;;when save file, update
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor nil
 helm-gtags-prefix-key nil
 helm-gtags-suggested-key-mapping nil
 )


(provide 'init-helm-gtags)
