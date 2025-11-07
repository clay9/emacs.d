;;; init-buffer.el --- Control buffer display and naming -*- lexical-binding: t -*-
;;; Commentary:
;; This file provides:
;;   - Display rules for special buffers (Help, Messages, Org Agenda, Shell)
;;   - Uniquify buffer names for files with identical names
;;   - File & buffer management utilities
;;; Code:

;; ----------------------
;;; Display buffer rules
;; ----------------------
;; Customize how Emacs displays certain buffers.
(add-to-list 'display-buffer-alist
             `("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\)\\*"
               (display-buffer-reuse-mode-window display-buffer-same-window)
               (inhibit-same-window . nil)
               (mode . help-mode)))

;; (add-to-list 'display-buffer-alist
;;              `("\\*e?shell\\*"
;;                (display-buffer-in-side-window)
;;                (window-height . 0.4)
;;                (side . bottom)
;;                (slot . -1)
;;                (window-parameters . ((no-other-window . t)
;;                                      (no-delete-other-windows . t)))))

;; ----------------------
;;; Uniquify buffer names
;; ----------------------
;; Make buffer names unique and more readable for files with identical names.
(use-package uniquify
  :defer t
  :ensure nil  ;; 防止 use-package 尝试从包管理器安装
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;; ----------------------
;;; File & buffer utilities
;; ----------------------
;; Provide helper functions for file and buffer management.
(defun my/delete-current-file ()
  "Delete the file associated with the current buffer and kill the buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (not file)
        (message "Buffer is not visiting a file!")
      (when (yes-or-no-p (format "Really delete file %s? " file))
        (let ((delete-by-moving-to-trash t))
          (delete-file file))
        (kill-current-buffer)
        (message "Deleted file %s" file)))))

(provide 'init-buffer)
;;; init-buffer.el ends here
