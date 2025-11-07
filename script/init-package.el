;;; init-package.el --- Control Packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Setup package management and use-package

(require 'package)

;; Package directory
(setq package-user-dir
      (expand-file-name
       (format ".elpa-%s.%s" emacs-major-version emacs-minor-version)
       user-emacs-directory))

;; Standard package repositories
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize package system
(setq package-enable-at-startup nil
      package-native-compile t)
(package-initialize)

;; Refresh package contents if not yet available
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed
(require 'use-package)
(setq use-package-always-ensure t)

(provide 'init-package)
;;; init-package.el ends here
