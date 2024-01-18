;;; init-text-snippet.el --- Settings for snippet -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  ;;; funs for yasnippet template
  (defun yas/hugo-get-bundle()
    (let* ((file-path (expand-file-name (buffer-file-name)))
           (file-dir (file-name-directory file-path))
           (index (string-match "org/" file-dir))
           (rpath (substring file-dir (+ index 4) -1)))
      rpath))
  (defun yas/hugo-get-bundle-name()
    (let* ((file-path (expand-file-name (buffer-file-name)))
           (file-name (file-name-sans-extension (file-name-nondirectory file-path))))
      file-name))
  :config
  (setq yas-snippet-dirs (list (expand-file-name ".yasnippet" user-emacs-directory)))
  ;; start must after settings, or settings will not work
  (yas-global-mode t))

(provide 'init-text-snippet)
;;; init-text-snippet.el ends here
