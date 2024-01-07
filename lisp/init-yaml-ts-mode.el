;;; init-yaml-ts-mode.el --- Support Yaml files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))


;;; install

(require 'treesit)
(when (not (treesit-ready-p 'yaml))
  ;; treesit language
  (add-to-list 'treesit-language-source-alist
               '(yaml "https://github.com/ikatyang/tree-sitter-yaml.git"))

  ;; install language gramar
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang)))))

(provide 'init-yaml-ts-mode)
;;; init-yaml-ts-mode.el ends here
