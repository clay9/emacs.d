;;; init-sessions.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -----------------------------
;; Desktop: Save open buffers & layout
;; -----------------------------
(setq desktop-path (list my/ecfg-dir)
      desktop-base-file-name "emacs.desktop"
      desktop-base-lock-name "emacs.desktop.lock"
      desktop-auto-save-timeout 600

      desktop-globals-to-save
      '((comint-input-ring        . 50)
        (compile-history          . 30)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 30)
        (face-name-history        . 20)
        (file-name-history        . 100)
        (grep-find-history        . 30)
        (grep-history             . 30)
        (ivy-history              . 100)
        (magit-revision-history   . 50)
        (minibuffer-history       . 50)
        (org-clock-history        . 50)
        (org-refile-history       . 50)
        (org-tags-history         . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 50)
        tags-file-name
        tags-table-list))
(desktop-save-mode 1)

;; -----------------------------
;; Session: Save minibuffer history, variables, etc.
;; -----------------------------
(use-package session
  :hook (after-init . session-initialize)
  :config
  (setq session-save-file (concat my/ecfg-dir "session"))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (setq session-save-file-coding-system 'utf-8)
  (setq history-length 200)
  (savehist-mode 1))

;; -----------------------------
;; Recentf: Keep track of recently opened files
;; -----------------------------
(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-save-file (concat my/ecfg-dir "recentf")
        recentf-max-saved-items 20
        recentf-exclude `("/tmp/"
                          "~/my/"
                          "~/.emacs.d/"
                          "/ssh:"
                          ,(concat package-user-dir "/.*-autoloads\\.el\\'"))))

;; -----------------------------
;; Auto-save sessions on exit
;; -----------------------------
(defun my/save-session-on-exit ()
  "Save desktop, session, and recentf before Emacs exits."
  (desktop-save-in-desktop-dir)
  (session-save-session)
  (recentf-save-list))
(add-hook 'kill-emacs-hook #'my/save-session-on-exit)

(provide 'init-sessions)
;;; init-sessions.el ends here
