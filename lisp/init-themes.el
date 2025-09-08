;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dracula-theme
  :config
  (condition-case nil
      (load-theme 'dracula t)
    (error (load-theme 'wombat t))))

;; set face
(with-eval-after-load 'hi-lock
  (set-face-attribute 'hi-yellow nil :background "color-29"))

(provide 'init-themes)
;;; init-themes.el ends here
