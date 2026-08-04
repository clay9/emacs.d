;;; init-dired-mode.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "t") #'dired-up-directory))

(provide 'init-dired-mode)
;;; init-dired-mode.el ends here
