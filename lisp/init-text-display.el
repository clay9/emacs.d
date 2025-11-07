;;; init-text-display.el --- Text display enhancements -*- lexical-binding: t -*-
;;; Commentary:
;; This file configures general text display and visual enhancements:
;;   - line wrapping
;;   - transient mark, electric pair, show-paren
;;   - delete selection, electric indent, eldoc
;;   - whitespace and carriage return cleanup
;;   - line numbers
;;; Code:

;; ----------------------
;;; Line truncation
;; ----------------------
(setq-default
 truncate-lines nil
 truncate-partial-width-windows nil)

;; ----------------------
;;; Basic visual enhancements
;; ----------------------
(dolist (mode '(transient-mark-mode
                electric-pair-mode
                show-paren-mode
                delete-selection-mode
                electric-indent-mode
                global-eldoc-mode))
  (add-hook 'after-init-hook mode))

(diminish 'eldoc-mode)

;; ----------------------
;;; Whitespace management
;; ----------------------
(add-hook 'find-file-hook
          (lambda ()
            "Show trailing whitespace only in writable file buffers."
            (when (and buffer-file-name
                       (not buffer-read-only))
              (setq show-trailing-whitespace t))))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'before-save-hook
          (lambda ()
            (save-excursion
              (goto-char (point-min))
              (while (search-forward "\r" nil t)
                (replace-match "")))))

;; ----------------------
;;; Line numbers
;; ----------------------
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))

(provide 'init-text-display)
;;; init-text-display.el ends here
