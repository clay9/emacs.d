;;; init-text-fold.el --- Text folding and block hiding -*- lexical-binding: t -*-
;;; Commentary:
;;; This file provides commands and keybindings for folding/hiding text:
;;;   - Outline minor mode toggles
;;;   - hideshow minor mode for code blocks
;;;   - Tree-sitter based folding for syntax-aware hiding
;;;   - Shift-TAB key for context-aware folding/unfolding
;;; Code:

;; ----------------------
;;; hideshow minor mode
;; ----------------------
(require 'init-text-fold-hs)

;; ----------------------
;;; Tree-sitter folding
;; ----------------------
(require 'init-text-fold-treesit)

;; ----------------------
;;; Outline minor mode toggles
;; ----------------------
(require 'init-text-fold-outline)

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
(defun text/shift-tab ()
  "Context-aware Shift-TAB for folding."
  (interactive)
  (cond ((bound-and-true-p treesit-fold-mode)
         (text/ts-shift-tab))
        ((bound-and-true-p hs-minor-mode)
         (text/hs-shift-tab))))

(provide 'init-text-fold)
;;; init-text-fold.el ends here
