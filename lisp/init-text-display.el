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
;; set  to horizontal line
(add-hook 'prog-mode-hook #'whitespace-page-delimiters-mode)
(add-hook 'text-mode-hook #'whitespace-page-delimiters-mode)
;; whitespace-page-delimiters-mode 会自动开启whitespace mode,
;; 因此这里修改下默认样式
(setq-default whitespace-style '(face page-delimiters))

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
