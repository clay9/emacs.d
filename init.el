;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;(setq debug-on-error t)

(defconst my/ecfg-dir (concat user-emacs-directory ".config/"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;;; Bootstrap config

(setq custom-file (concat my/ecfg-dir "custom.el"))
(require 'init-site-lisp)
(require 'init-package)

;; emacs-progn basic
(require 'init-themes)
(require 'init-gui-frames)
(require 'init-font)
(require 'init-windows)
(require 'init-buffers)
(require 'init-recentf)
(require 'init-sessions)
(require 'init-tty-keys)

;; text editor
(require 'init-text-show)
(require 'init-text-move-and-kill)
(require 'init-text-hs)
(require 'init-text-snippet)
(require 'init-completion)

(require 'init-global-shortkey)

;; progn editor
(require 'init-outline)
(require 'init-flymake)
(require 'init-eglot)
(require 'init-compile)
(require 'init-gdb)
(require 'init-ai)

;; project
(require 'init-git)
(require 'init-project)

;; major mode
(require 'init-tree-sitter)
(require 'init-artist)
(require 'init-c++-ts-mode)
(require 'init-dockerfile-ts-mode)
(require 'init-yaml-mode)
(require 'init-protobuf-mode)
(require 'init-plantuml)
(require 'init-emacs-lisp-mode)
(require 'init-help-mode)
(require 'init-org-mode)
(require 'init-org-agenda-mode)

;; tools
(require 'init-term)
(require 'init-eshell)
(require 'init-api)
;(require 'init-translate)
;;(require 'init-gnus)


;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file) (load custom-file))
;; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)

(provide 'init)
;;; init.el ends here
