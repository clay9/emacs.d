;;; init-session.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;; Save and restore editor sessions between restarts:
;;   - desktop.el : open buffers & window layout
;;   - savehist.el: minibuffer history and user histories
;;; Code:

;; ----------------------
;;; desktop.el: Save open buffers & layout
;; ----------------------
(setq desktop-path (list my/config-dir)
      desktop-base-file-name "emacs.desktop"
      desktop-base-lock-name "emacs.desktop.lock"
      desktop-auto-save-timeout 600
      desktop-globals-to-save
      '(desktop-missing-file-warning
        register-alist))
(desktop-save-mode 1)

;; ----------------------
;;; savehist.el: Save history
;; ----------------------
(setq savehist-file (expand-file-name "history" my/config-dir)
      history-length 200
      savehist-additional-variables
      '(comint-input-ring
        compile-history
        regexp-search-ring
        search-ring
        shell-command-history))
(savehist-mode 1)


(provide 'init-session)
;;; init-session.el ends here
