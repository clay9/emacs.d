;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;(setq debug-on-error t)

;;; Constants
(defconst my/ecfg-dir (expand-file-name ".config/" user-emacs-directory)
  "Directory for custom Emacs configuration files.")

(unless (file-directory-p my/ecfg-dir) (make-directory my/ecfg-dir))

(setq custom-file (concat my/ecfg-dir "custom.el"))

;;; Bootstrap config
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-site-lisp)
(require 'init-package)

;;; Emacs core setup
(require 'init-themes)
(require 'init-font)
(require 'init-gui-frame)

(require 'init-window)
(require 'init-buffer)
(require 'init-session)
(require 'init-terminal-keys)

;;; Text editor enhancements
(require 'init-text-display)
(require 'init-text-commands)
(require 'init-text-fold)
(require 'init-text-snippet)
(require 'init-text-grep)

(require 'init-vertico-suite)
(require 'init-corfu)

(require 'init-global-keybindings)

;;; Programming editor features
(require 'init-flymake)
(require 'init-eglot)
(require 'init-compile)
(require 'init-gdb)  ;; TODONOW 改用lap-mode
;; (require 'init-ai-assistant)

;;; Project management
(require 'init-git)
(require 'init-project)

;;; Major modes
(require 'init-c++-ts-mode)
(require 'init-cmake-ts-mode)
(require 'init-dockerfile-ts-mode)
(require 'init-yaml-ts-mode)
(require 'init-toml-ts-mode)
(require 'init-protobuf-mode)
(require 'init-emacs-lisp-mode)
(require 'init-help-mode)
(require 'init-org-mode)
(require 'init-org-agenda-mode)
(require 'init-sqlite-mode)

(require 'init-artist-mode)
(require 'init-mermaid-mode)

;;; Tools
(require 'init-ansi-term)
(require 'init-eshell)
(require 'init-devdocs)
(require 'init-journal)
(require 'init-input-method)

;;; Load custom variables if present
(when (file-exists-p custom-file)
  (load custom-file))

;;; Locales (must be set after loading custom-file)
(require 'init-locales)

(provide 'init)
;;; init.el ends here
