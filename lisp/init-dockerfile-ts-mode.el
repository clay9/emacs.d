;;; init-dockerfile-ts-mode.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist
             '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'"
               . dockerfile-ts-mode))


;;; install

(require 'treesit)
(when (not (treesit-ready-p 'dockerfile))
  ;; treesit language
  (add-to-list 'treesit-language-source-alist
               '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile"))
  
  ;; install language gramar
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang)))))

(provide 'init-dockerfile-ts-mode)
;;; init-dockerfile-ts-mode.el ends here
