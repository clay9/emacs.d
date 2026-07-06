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
;; global mode
(transient-mark-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(electric-indent-mode 1)
(global-eldoc-mode 1)

;; local mode
;; set  (C-q C-l) to horizontal line
;; bug: 与 display-number-mode 冲突
;; (add-hook 'prog-mode-hook #'whitespace-page-delimiters-mode)
;; (add-hook 'text-mode-hook #'whitespace-page-delimiters-mode)

;; Hide ElDoc indicator in the mode line.
(with-eval-after-load 'eldoc
  (when-let* ((cell (assq 'eldoc-mode minor-mode-alist)))
    (setcdr cell '(""))))

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
(setq-default display-line-numbers-width 3)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)


(provide 'init-text-display)
;;; init-text-display.el ends here
