;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dracula-theme
  :config
  ;; If you don't customize it, this is the theme you get.
  (setq-default custom-enabled-themes '(dracula))

  ;; Ensure that themes will be applied even if they have not been customized
  (defun reapply-themes ()
    "Forcibly load the themes listed in `custom-enabled-themes'."
    (dolist (theme custom-enabled-themes)
      (unless (custom-theme-p theme)
	(load-theme theme)))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

  (add-hook 'after-init-hook 'reapply-themes))

;; set face
(with-eval-after-load 'hi-lock
  (set-face-attribute 'hi-yellow nil :background "color-29"))

(provide 'init-themes)
;;; init-themes.el ends here
