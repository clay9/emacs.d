;;; init-eshell.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package eshell
  :config
  (setq eshell-directory-name (concat my/ecfg-dir "eshell/")))

(with-eval-after-load 'esh-mode
  (fmakunbound 'eshell/clear-scrollback)
  
  (defun eshell/clear ()
    "Override => use eshell/clear-scrollback"
    (let ((inhibit-read-only t))
      (erase-buffer))))


(provide 'init-eshell)
;;; init-eshell.el ends here
