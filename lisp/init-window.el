;;; init-window.el --- Working with window within frames -*- lexical-binding: t -*-
;;; Commentary:
;; This file configures Emacs "window" — the panels within a frame that
;; display buffers. It provides:
;;   - Window layout management (winner-mode)
;;   - Window switching enhancements (switch-window)
;;   - Split windows with other-buffer behavior
;;   - Toggle delete/restore other windows
;;   - Toggle window dedication
;;   - Save/restore window configurations
;;; Code:

;; ----------------------
;;; Enable winner-mode
;; ----------------------
;; Winner mode allows undo/redo of window configurations with C-c <left> / C-c <right>
(winner-mode 1)

;; ----------------------
;;; Switch window
;; ----------------------
;; Use switch-window to display overlays in each window when switching.
;; Makes "C-x o" easier to use when multiple windows exist.
(use-package switch-window
  :config
  (setq-default switch-window-shortcut-style 'alphabet
                switch-window-timeout nil)
  (defun win/switch-window ()
    "Display overlay keys in each window and prompt user to select one.
Keys: i-j-k-l to select, b to reset."
    (interactive)
    (switch-window--then "[Move: i-j-k-l, Reset: b] Move to: "
                         #'switch-window--other-window-or-frame)))

;; ----------------------
;;; Split windows with other buffer
;; ----------------------
;; Provides a wrapper to split a window and display the "other buffer"
;; in the new window. Optionally select the new window unless ARG is non-nil.
(defun win/split-window-with-other-buffer (split-fn)
  "Return a command that splits window using SPLIT-FN and shows the other buffer."
  (lambda (&optional arg)
    (interactive "P")
    (funcall split-fn)
    (let ((target (next-window)))
      (set-window-buffer target (other-buffer))
      (unless arg
        (select-window target)))))

;; ----------------------
;;; Toggle delete/restore other windows
;; ----------------------
;; Deletes other windows if multiple, or restores previous layout if only one.
(defun win/toggle-delete-other-windows ()
  "Delete other windows or undo if only one window remains."
  (interactive)
  (if (and winner-mode (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

;; ----------------------
;;; Dedicated window toggle
;; ----------------------
;; Makes the current window "dedicated" to its buffer or un-dedicated.
;; A dedicated window won't be reused by `display-buffer`.
(defun win/toggle-current-window-dedication ()
  "Toggle whether current window is dedicated to its buffer."
  (interactive)
  (let* ((win (selected-window))
         (dedicated (window-dedicated-p win)))
    (set-window-dedicated-p win (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

;; ----------------------
;;; Save / restore window configurations
;; ----------------------
;; Stack-based saving and restoring of window layouts.
;; Useful for temporary layouts or experiments with window splits.
(defvar win/config-stack nil
  "Stack of saved window configurations.")

(defun win/save-window-configuration ()
  "Push current window configuration onto the stack."
  (interactive)
  (push (current-window-configuration) win/config-stack))

(defun win/restore-window-configuration ()
  "Pop and restore the most recent window configuration.
If the stack is empty, display a message."
  (interactive)
  (let ((cfg (pop win/config-stack)))
    (if cfg
        (set-window-configuration cfg)
      (message "No saved window configuration."))))

(defun win/window-configuration-stack ()
  "Return the current stack of saved window configurations."
  win/config-stack)

(provide 'init-window)
;;; init-windows.el ends here
