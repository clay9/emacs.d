;;; init-package.el --- Crontrol Packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

;; package dir
(setq package-user-dir
      (expand-file-name
       (format ".elpa-%s.%s" emacs-major-version emacs-minor-version)
       user-emacs-directory))

;; Standard package repositories
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)

;; Fire up package.el
(setq package-enable-at-startup nil)
(setq package-native-compile t)
(package-initialize)

(unless (file-exists-p (expand-file-name "archives" package-user-dir))
  (package-refresh-contents))


;; use package
(require 'use-package)
(require 'use-package-ensure)

(setq use-package-always-ensure t)

;; transient
(require 'transient)

;; diminish mode-line
(use-package diminish)

(provide 'init-package)
;;; init-package.el ends here
