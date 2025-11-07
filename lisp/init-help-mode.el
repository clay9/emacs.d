;;; init-help-mode.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -----------------------------
;;; 快捷键
;; -----------------------------
(with-eval-after-load 'help-mode
  (define-key help-mode-map (kbd "<return>")
              (lambda()
                (interactive)
		(windmove-display-same-window)
		(push-button)))
  (define-key help-mode-map (kbd "n") 'forward-button)
  (define-key help-mode-map (kbd "p") 'backward-button)
  (define-key help-mode-map (kbd "b") 'help-go-back)
  (define-key help-mode-map (kbd "f") 'help-go-forward))

(provide 'init-help-mode)
;;; init-help-mode.el ends here
