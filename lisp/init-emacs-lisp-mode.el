;;; init-emacs-lisp-mode.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'elisp-mode
  (define-key lisp-interaction-mode-map (kbd "C-x C-e") 'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'eval-defun))

(provide 'init-emacs-lisp-mode)
;;; init-emacs-lisp-mode.el ends here
