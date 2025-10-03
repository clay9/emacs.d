;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;;(setq debug-on-error t)

;;; Constants
(defconst my/ecfg-dir
  (expand-file-name ".config/" user-emacs-directory)
  "Directory for custom Emacs configuration files.")

(setq custom-file (concat my/ecfg-dir "custom.el"))

;;; Bootstrap config
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-site-lisp)
(require 'init-package)

;;; Emacs core setup
(require 'init-themes)
(require 'init-font)
(require 'init-gui-frame)

(require 'init-window)  ;; TODO 从这里开始, 优化了注释修改. 之前的文件需要同步
(require 'init-buffer)
(require 'init-session)
(require 'init-terminal-keys)

;;; Text editor enhancements
(require 'init-text-display)
(require 'init-text-commands)
(require 'init-text-fold)  ;; TODO 整理到这里了
(require 'init-text-snippet)
(require 'init-completion)

(require 'init-global-shortkey)

;;; Programming editor features
(require 'init-outline)
(require 'init-flymake)
(require 'init-eglot)
(require 'init-compile)
(require 'init-gdb)
(require 'init-ai)

;;; Project management
(require 'init-git)
(require 'init-project)

;;; Major modes
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

;;; Tools
(require 'init-term)
(require 'init-eshell)
(require 'init-api)

;;; Load custom variables if present
(when (file-exists-p custom-file)
  (load custom-file))

;;; Locales (must be set after loading custom-file)
(require 'init-locales)

(provide 'init)
;;; init.el ends here
