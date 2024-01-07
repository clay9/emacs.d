;;; init-translate.el --- translate  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package go-translate
  :commands (my/translate-at-point my/translate-buffer)
  :config
  (use-package posframe)
  
  (setq gts-translate-list '(("en" "zh")))
  (setq gts-cache-enable nil)

  (defvar my-translator-at-point
    (gts-translator
     :picker (gts-noprompt-picker)
     :engines (list (gts-bing-engine))
     :render (gts-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff")))

  (defvar my-translator-buffer
    (gts-translator
     :picker (gts-noprompt-picker :texter (gts-whole-buffer-texter) :single t)
     :engines (list (gts-bing-engine))
     :render (gts-posframe-pop-render :backcolor "#333333" :forecolor "#ffffff")))

  (defun my/translate-at-point()
    (interactive)
    (gts-translate my-translator-at-point))

  (defun my/translate-buffer()
    (interactive)
    (gts-translate my-translator-buffer)))

(provide 'init-translate)
;;; init-translate.el ends here
