;;; sub-gtd-clock-display.el --- Org Clock Display -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org)

(use-package posframe
  :ensure t
  :config
  (defvar my/org-clock-posframe-buffer " *org-clock-posframe*"
    "The posframe buffer used to display the current Org Clock status.")

  (defvar my/org-clock-timer nil
    "Timer object for updating org-clock posframe every 30 seconds.")

  (defun my/org-clock-posframe-update ()
    "Show or update the Org Clock status posframe near the title bar.
If clocking is active, display the current clock string in a small
floating frame positioned near the top-right corner of the Emacs frame.
If not clocking, hide the posframe."
    (let* ((clock-string (format "🕒%s" (org-clock-get-clock-string)))
           ;; Estimate posframe width in pixels based on string length.
           (fw (frame-pixel-width))
           (pw (string-pixel-width clock-string))
           ;; Calculate manual position to ensure consistent placement.
           (x (max 0 (- fw pw 0)))    ;; 30px margin from the right edge
           (y 0))                     ;; 10px margin from the top edge
      (posframe-show
       my/org-clock-posframe-buffer
       ;; :string clock-string
       :string (propertize clock-string 'face `(:foreground "#1b5e20"))
       :position (cons x y)
       :background-color "#d9f7be"
       :accept-focus nil
       :override-parameters
       '((undecorated . t)
         (minibuffer . nil)
         (menu-bar-lines . 0)
         (tool-bar-lines . 0)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (no-other-frame . t)))))

  (defun my/org-clock-posframe-start ()
    (unless (timerp my/org-clock-timer)
      (setq my/org-clock-timer
            (run-with-timer 0 59 #'my/org-clock-posframe-update)))
    (my/org-clock-posframe-update))

  (defun my/org-clock-posframe-stop ()
    (when (timerp my/org-clock-timer)
      (cancel-timer my/org-clock-timer)
      (setq my/org-clock-timer nil))
    (posframe-hide my/org-clock-posframe-buffer))

  ;; Automatically show/hide the posframe when clock starts/stops
  (add-hook 'org-clock-in-hook #'my/org-clock-posframe-start)
  (add-hook 'org-clock-out-hook #'my/org-clock-posframe-stop)
  (add-hook 'org-clock-cancel-hook #'my/org-clock-posframe-stop))

;; Doesn't show clock in mode line
(setq org-clock-clocked-in-display nil)

(provide 'sub-gtd-clock-display)
;;; sub-gtd-clock-display.el ends here
