;;; init-text-commands.el --- Text editing commands -*- lexical-binding: t -*-
;;; Commentary:
;; This file provides enhanced text editing operations:
;;   - Delete thing at point (char, word, sexp, line)
;;   - Kill / kill-save region
;;   - Yank and yank-pop enhancements
;;   - Expand-region support
;;; Code:

;; ----------------------
;;; Delete operations
;; ----------------------
(defun text/delete-thing-at-point (thing)
  "Delete THING at point, returns t if deleted."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (progn
          (delete-region (car bounds) (cdr bounds))
          t)
      nil)))

(defun text/delete ()
  "Delete with priority: beginning-of-line > word > sexp > whitespace."
  (interactive)
  (let* ((is-char-empty (or (equal 32 (char-after))
                            (equal 9 (char-after))))
         (is-char-before-empty (or (equal 32 (char-before))
                                   (equal 9 (char-before))))
         (is-whitespace (and is-char-empty is-char-before-empty)))
    (cond
     ((bolp)
      (text/delete-thing-at-point 'line))
     (is-whitespace
      (delete-all-space)
      (insert " "))
     ((bounds-of-thing-at-point 'word)
      (text/delete-thing-at-point 'word))
     ((bounds-of-thing-at-point 'sexp)
      (text/delete-thing-at-point 'sexp)))))

;; ----------------------
;;; Kill region
;; ----------------------
;; 因为mark不可见, 导致kill-region时候会有奇怪现象
;; 因此只对active-region进行kill, kill-save
;; active target object:
;; 1. word, sentence, sexp => my/expand-region, my/contract-region
;; 2. line => C-a, 按住C-, C-e, 释放C-. 进行选中

(defun text/kill ()
  "Kill the active region. Error if no active region."
  (interactive)
  (if (not (use-region-p))
      (error "No active region")
    (call-interactively 'kill-region)))

(defun text/kill-save ()
  "Copy (kill-ring-save) the active region. Error if no active region."
  (interactive)
  (if (not (use-region-p))
      (error "No active region")
    (call-interactively 'kill-ring-save)))

;; ----------------------
;;; Yank operations
;; ----------------------
(defvar text/last-yank-point 0)
(defvar text/yank-times 1)

(defun text/yank-pop ()
  "Enhanced yank-pop supporting up to 3 repetitions or consult-yank."
  (interactive)
  (let* ((yank-p (/= (point) text/last-yank-point))
         (yank-pop-p (< text/yank-times 3)))
    (if yank-p
        (setq text/yank-times 1)
      (setq text/yank-times (1+ text/yank-times)))
    (if yank-p
        (yank)
      (if yank-pop-p
          (yank-pop 1)
        (delete-region (mark t) (point))
        (call-interactively 'consult-yank-from-kill-ring))))
  (setq text/last-yank-point (point)))

;; ----------------------
;;; Expand region
;; ----------------------
(use-package expand-region
  :commands (er/expand-region er/contract-region))

(provide 'init-text-commands)
;;; init-text-commands.el ends here
