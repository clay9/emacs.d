;;; init-for-script.el ---   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "script" user-emacs-directory))
(require 'init-package)

(require 'export-org-to-md)

;;; init-for-script.el ends here
