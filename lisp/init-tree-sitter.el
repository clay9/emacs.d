;;; init-tree-sitter.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (eq system-type 'windows-nt)
  (setq treesit-extra-load-path (list (concat user-emacs-directory "tree-sitter/" "windows"))))

(when (eq system-type 'darwin)
  (setq treesit-extra-load-path (list (concat user-emacs-directory "tree-sitter/" "macos"))))

(provide 'init-tree-sitter)
;;; init-tree-sitter.el ends here
