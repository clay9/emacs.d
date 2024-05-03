;;; init-buffers.el --- Ctrl buffers format -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ctrl buffer display
(add-to-list 'display-buffer-alist
	     `("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\)\\*"
	       (display-buffer-reuse-mode-window  display-buffer-same-window)
	       (inhibit-same-window . nil)
	       (mode . help-mode)))

(add-to-list 'display-buffer-alist
	     `("\\*Org Agenda\\*"
               (display-buffer-in-side-window)
	       (window-width . 0.4)
	       (side . left)
	       (slot . -1)
               (dedicated . nil)))

(add-to-list 'display-buffer-alist
	     `("\\*e?shell\\*"
	       (display-buffer-in-side-window)
	       (window-height . 0.4)
	       (side . bottom)
	       (slot . -1)
	       (window-parameters . ((no-other-window . t)
				     (no-delete-other-windows . t)))))


;; Nicer naming of buffers for files with identical names
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


;; file && buffer
(defun my/delete-current-file ()
  (interactive)
  (let ((delete-by-moving-to-trash t))
    (delete-file (buffer-file-name)))
  (kill-current-buffer))


(provide 'init-buffers)
;;; init-buffers.el ends here
