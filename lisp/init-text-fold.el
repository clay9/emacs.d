;;; init-text-fold.el --- Text folding and block hiding -*- lexical-binding: t -*-
;;; Commentary:
;; This file provides commands and keybindings for folding/hiding text:
;;   - If 'treesit-fold' is available, use it. Otherwise, fallback to 'hs-minor-mode'
;;   - Outline minor modeg
;;; Code:

;; ----------------------
;;; Tree-sitter folding
;; ----------------------
(require 'sub-text-fold-treesit)

;; ----------------------
;;; hideshow minor mode
;; ----------------------
(require 'sub-text-fold-hs)

;; ----------------------
;;; Outline minor mode toggles
;; ----------------------
(require 'sub-text-fold-outline)

;; ----------------------
;;; Hooks
;; ----------------------
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (treesit-parser-list)
              (hs-minor-mode))))

;; ----------------------
;;; Keybinding
;; ----------------------
(defun text/fold ()
  "Context-aware Shift-TAB for folding."
  (interactive)
  (cond ((bound-and-true-p treesit-fold-mode)
         (text/treesit-fold-cycle))
        ((bound-and-true-p hs-minor-mode)
         (text/hs-cycle))))

(provide 'init-text-fold)
;;; init-text-fold.el ends here
