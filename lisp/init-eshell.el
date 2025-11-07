;;; init-eshell.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eshell
  :ensure nil
  :commands (eshell)
  :config
  (setq eshell-directory-name (concat my/ecfg-dir "eshell/")
        eshell-input-filter 'my/eshell-input-filter)

  (defun my/eshell-input-filter (input)
    "Do not save on the following:
       - empty lines
       - commands that starts with a space, `cd', `ls' ..."
    (and
     (eshell-input-filter-default input)
     (eshell-input-filter-initial-space input)
     (not (string-prefix-p "cd " input))
     (not (string-prefix-p "ls" input))
     (not (string-prefix-p "clear" input))
     (not (string-prefix-p "pwd" input))
     (not (string-prefix-p "exit" input))
     (not (string-prefix-p "history" input))))

  (with-eval-after-load 'esh-mode
    ;; cancel 'clear-scrollback'
    (fmakunbound 'eshell/clear-scrollback)

    ;; override 'eshell/clear'
    (defun eshell/clear ()
      "Override eshell/clear-scrollback: clear buffer"
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(provide 'init-eshell)
;;; init-eshell.el ends here
