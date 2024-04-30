;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;(setq debug-on-error t)

(defconst my/ecfg-dir (concat user-emacs-directory ".config/"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


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
(require 'init-flymake)
(require 'init-eglot)
(require 'init-compile)
;;(require 'init-gdb) ;; TODO re-write
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
(require 'init-screencast)
(require 'init-translate)
;;(require 'init-gnus)


;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file) (load custom-file))
;; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)

(provide 'init)
;;; init.el ends here
