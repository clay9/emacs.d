;;; init-session.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;; This file manages saving and restoring Emacs sessions between restarts:
;;   - Desktop: buffers & window layout
;;   - Session: minibuffer history and variables
;;   - Auto-save session on exit
;;; Code:

;; ----------------------
;;; Desktop: Save open buffers & layout
;; ----------------------
;; Keep track of open buffers and window layouts.
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

;; ----------------------
;;; Session: Save minibuffer history, variables, etc.
;; ----------------------
;; Session package manages minibuffer history, etc.
(use-package session
  :defer t
  :hook (after-init . session-initialize)
  :config
  (setq session-save-file (concat my/ecfg-dir "session")
        session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)"
        session-save-file-coding-system 'utf-8
        history-length 200)
  (savehist-mode 1))

;; ----------------------
;;; Auto-save sessions on exit
;; ----------------------
;; Save desktop, session, and recentf before Emacs exits.
(add-hook 'kill-emacs-hook
          (lambda ()
            (desktop-save-in-desktop-dir)
            (session-save-session)))

(provide 'init-session)
;;; init-session.el ends here
