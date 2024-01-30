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
           (rpath (substring file-dir (+ index 4) -1))
           (file-name (file-name-sans-extension (file-name-nondirectory file-path))))
      (if (string= file-name "_index")
          rpath
        (concat rpath "/" file-name))))
  (defun yas/hugo-get-bundle-name()
    (let* ((file-path (expand-file-name (buffer-file-name)))
           (file-name (file-name-sans-extension (file-name-nondirectory file-path))))
      (if (string= file-name "_index")
          "_index"
        "index")))
  (defun yas/hugo-get-collapse-section()
    (let* ((file-path (expand-file-name (buffer-file-name)))
           (file-name (file-name-sans-extension (file-name-nondirectory file-path))))
      (if (string= file-name "_index")
          "true"
        "false")))
  :config
  (setq yas-snippet-dirs (list (expand-file-name ".yasnippet" user-emacs-directory)))
  ;; start must after settings, or settings will not work
  (yas-global-mode t))

(provide 'init-text-snippet)
;;; init-text-snippet.el ends here
