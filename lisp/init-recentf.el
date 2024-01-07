;;; init-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-save-file (concat my/ecfg-dir "recentf")
 recentf-max-saved-items 20
 recentf-exclude `("/tmp/" "~/my/" "~/.emacs.d/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))


(provide 'init-recentf)
;;; init-recentf.el ends here
