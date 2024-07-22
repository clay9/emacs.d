;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Some basic preferences

(tool-bar-mode -1)
(menu-bar-mode -1)
(when (display-graphic-p)
  (set-scroll-bar-mode nil))
;;(setq default-frame-alist '((undecorated . t)))

(setq-default
 use-file-dialog nil
 use-dialog-box nil
 inhibit-startup-screen t
 gnus-inhibit-startup-message t

 window-resize-pixelwise t
 frame-resize-pixelwise t

 make-backup-files nil
 auto-save-default nil
 auth-sources  "~/my/gtd/.emacs_cfg/authinfo"
 bookmark-default-file "~/my/gtd/.emacs_cfg/bookmark"
 transient-save-history nil
 savehist-file (concat my/ecfg-dir "save-history")
 auto-save-list-file-prefix (concat my/ecfg-dir "auto-save-list/saves-")
 tramp-persistency-file-name (concat my/ecfg-dir "tramp")
 tramp-verbose 1
 initial-scratch-message ";; Emacs is just editor, not system !!!\n\n"

 speedbar-use-images nil
 ring-bell-function 'ignore
 case-fold-search t
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t

 enable-recursive-minibuffers t
 system-time-locale "C")

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(fset 'yes-or-no-p 'y-or-n-p)


;; window
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))


;; disable mouse
(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :config
  (global-disable-mouse-mode 1))


(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
