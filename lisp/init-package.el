;;; init-package.el --- Crontrol Packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

;; package dir
(setq package-user-dir (expand-file-name (format ".elpa-%s.%s" emacs-major-version emacs-minor-version)
                                         user-emacs-directory))
;; Standard package repositories
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)


;;; use package

(require 'use-package)
(require 'use-package-ensure)

;; always ensure
(setq use-package-always-ensure t)

;; ensure system package
(use-package use-package-ensure-system-package)


;; diminish mode-line
(use-package diminish)

(provide 'init-package)
;;; init-package.el ends here
