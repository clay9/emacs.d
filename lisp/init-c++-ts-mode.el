;;; init-c++-ts-mode.el ---   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; TODO 这是emacs临时的解决方案, 等tree sitter在emacs成熟的时候, 应该会改掉
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist
             '(c-or-c++-mode . c-or-c++-ts-mode))


;;; k&r style
(with-eval-after-load 'c-ts-mode
  (setq c-ts-mode-indent-offset 4)
  
  ;; (defun kr-indent-style()
  ;;   "Override the built-in BSD indentation style with some additional rules."
  ;;   `(;; Here are your custom rules
  ;;     ((node-is ")") parent-bol 0)
  ;;     ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
  ;;     ((parent-is "argument_list") prev-sibling 0)
  ;;     ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
  ;;     ((parent-is "parameter_list") prev-sibling 0)

  ;;     ;; Append here the indent style you want as base
  ;;     ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))
  ;; (setq c-ts-mode-indent-style #'my-indent-style)
  )


;;; key
(with-eval-after-load 'c-ts-mode
  (transient-define-prefix my/transient/c++-ts-mode()
    [[:class transient-column "nav"
	     ("j" "definition" xref-find-definitions)
	     ("k" "references" xref-find-references)
	     ("f" "go-forward" xref-go-forward)
	     ("b" "go-back" xref-go-back)]
     
     [:class transient-column "rename"
	     ("r" "rename" eglot-rename)]])
  (define-key c++-ts-mode-map (kbd "C-j") 'my/transient/c++-ts-mode))


;;; install

(require 'treesit)
(when (not (treesit-ready-p 'cpp))
  ;; treesit language
  (add-to-list 'treesit-language-source-alist
               '(cpp "https://github.com/tree-sitter/tree-sitter-cpp"))
  (add-to-list 'treesit-language-source-alist
               '(c "https://github.com/tree-sitter/tree-sitter-c"))


  ;; install language gramar
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang))))

  ;; map tree_sitter_<LANGUAGE1> with libtree-sitter-<LANGUAGE2>.so
  (add-to-list 'treesit-load-name-override-list
               '(c++ "libtree-sitter-cpp")))

(provide 'init-c++-ts-mode)
;;; init-c++-ts-mode.el ends here
