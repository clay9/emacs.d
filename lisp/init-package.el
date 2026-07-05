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
      '(;;("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

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

;; Optional: manage transient with use-package
(use-package transient
  :ensure t
  :init
  (let ((dir (expand-file-name "cache/transient/" my/config-dir)))
    (setq transient-history-file (expand-file-name "history.el" dir)
          transient-values-file  (expand-file-name "values.el" dir)
          transient-levels-file (expand-file-name "levels.el" dir))))

;; Configure `project.el` before it is first loaded.
(use-package project
  :ensure nil
  :init
  (setq project-list-file
        (expand-file-name "projects.eld" my/config-dir)))

(provide 'init-package)
;;; init-package.el ends here
