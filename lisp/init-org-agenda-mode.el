;;; init-org-agenda-mode.el --- Org-agenda -*- lexical-binding: t -*-
;;; Commentary:
;; Org-agenda configuration for GTD workflow
;;; Code:

(require 'init-gtd-basic)    ;; GTD基础概念
(require 'init-gtd-workflow) ;; GTD流程设置
(require 'init-gtd-agenda)   ;; GTD可视化设置

(require 'sub-org-agenda-mode-keybindings)

;;----------------------------------------
;;; Org columns
;;----------------------------------------
(with-eval-after-load 'org-colview
  (setq org-columns-default-format "%24ITEM %7TODO %1PRIORITY %10TAGS %Effort{:} %10CLOCKSUM"))

;;----------------------------------------
;;; Save all Org buffers on exit
;;----------------------------------------
;; TODONOW not work
(with-eval-after-load 'org
  (add-hook 'kill-emacs-hook 'org-save-all-org-buffers))

;;----------------------------------------
;;; Auto show org-agenda day view
;;----------------------------------------
(use-package org-agenda
  :ensure nil
  :config
  (defun my/maybe-org-agenda-a ()
    "If org-agenda day view has items, show it; otherwise close and restore windows."
    (interactive)
    (unless (buffer-live-p (get-buffer "*Org Agenda*"))
      (win/save-window-configuration)
      (org-agenda nil "a")
      (message "call org-agenda-view")

      (unless (my/org-agenda-day-view-has-items)
        (kill-buffer "*Org Agenda*")
        (win/restore-window-configuration)
        (message "org-agenda-view has no items, quit."))))

  ;; emacs 启动完成的时候, 执行一次
  (add-hook 'emacs-startup-hook #'my/maybe-org-agenda-a)
  ;; 每次 Emacs 空闲 20 分钟执行一次
  (run-with-idle-timer 1200 t #'my/maybe-org-agenda-a))


(provide 'init-org-agenda-mode)
;;; init-org-agenda-mode.el ends here
