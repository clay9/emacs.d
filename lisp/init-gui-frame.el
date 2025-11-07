;;; init-gui-frame.el --- GUI / frame specific settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Configure graphical frame, window, and basic preferences
;;; Code:

;; Disable GUI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (display-graphic-p)
  (set-scroll-bar-mode nil))

;; Default frame settings
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; Set default behavior for window and frame resizing.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; Global editing preferences
(setq-default
 inhibit-startup-screen t
 initial-scratch-message nil
 use-file-dialog nil
 use-dialog-box nil
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil
 indent-tabs-mode nil
 column-number-mode t
 enable-recursive-minibuffers t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 system-time-locale "C"
 case-fold-search t
 save-interprogram-paste-before-kill t)

;; File and package related paths
(setq auth-sources  "~/my/gtd/.emacs_cfg/authinfo"
      bookmark-default-file "~/my/gtd/.emacs_cfg/bookmark"
      savehist-file (expand-file-name "save-history" my/ecfg-dir )
      auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" my/ecfg-dir)
      tramp-persistency-file-name (expand-file-name "tramp" my/ecfg-dir)
      tramp-verbose 1)

;; Misc
(setq speedbar-use-images nil
      ring-bell-function 'ignore)

;; Ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; Global minor modes
(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

;; Simplify prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Find file at point
(ffap-bindings)

;; Disable mouse globally
(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :config
  (global-disable-mouse-mode 1))

(provide 'init-gui-frame)
;;; init-gui-frame.el ends here
