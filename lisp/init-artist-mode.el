;;; init-artist-mode.el --- Artist mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package artist
  :commands artist-mode
  ;; :bind ( :map artist-mode-map
  ;;         ("C-<return>" . org-newline-and-indent)
  ;;         ("C-k" . org-kill-line))
  :config
  ;; 与 Org-mode 保持一致
  (add-hook 'artist-mode-hook
            (lambda ()
              (when (derived-mode-p 'org-mode)
                (setq tab-width tab-width
                      indent-tabs-mode indent-tabs-mode)))))

(provide 'init-artist-mode)
;;; init-artist-mode.el ends here
