;;; init-package.el --- Crontrol Packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

;; package dir
(setq package-user-dir (expand-file-name (format ".elpa-%s.%s" emacs-major-version emacs-minor-version)
                                         user-emacs-directory))
;; Standard package repositories
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '( "melpa" . "https://www.mirrorservice.org/sites/melpa.org/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (when (stringp min-version)
    (setq min-version (version-to-list min-version)))
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (best (car (sort known (lambda (a b)
                                      (version-list-<= (package-desc-version b)
                                                       (package-desc-version a)))))))
        (if (and best (version-list-<= min-version (package-desc-version best)))
            (package-install best)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t)))
        (package-installed-p package min-version))))


;;; use package

(require-package 'use-package)

;; always ensure
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; ensure system package
(use-package use-package-ensure-system-package)


;; diminish mode-line
(use-package diminish)

(provide 'init-package)
;;; init-package.el ends here
