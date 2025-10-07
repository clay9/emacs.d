;;; init-global-keybindings.el --- Global keybindings -*- lexical-binding: t -*-
;;; Commentary:
;; Defines global keybindings for windows, buffers, files, text operations,
;; embark, org-mode, rproject commands, tools, and transient prefixes.
;;; Code:

;; =====================
;;; Window Shortcuts
;; =====================
(global-set-key (kbd "C-x o") 'win/switch-window)
(global-set-key (kbd "C-x 1") 'win/toggle-delete-other-windows)
(global-set-key (kbd "C-x 2") (win/split-window-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (win/split-window-with-other-buffer 'split-window-horizontally))
(global-set-key (kbd "C-x t") 'win/toggle-current-window-dedication)

;; =====================
;;; Buffer Shortcuts
;; =====================
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x C-b") 'consult-project-buffer)
(global-set-key (kbd "C-x a") 'beginning-of-buffer)
(global-set-key (kbd "C-x e") 'end-of-buffer)
(global-set-key (kbd "C-x s") 'save-buffer)
(global-set-key (kbd "M-k") 'kill-current-buffer)

;; =====================
;;; File Shortcuts
;; =====================
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x C-f") 'project-find-file)

;; =====================
;;; Text Operations
;; =====================
(global-set-key (kbd "C-<backspace>") 'text/delete)
(global-set-key (kbd "C-k") 'text/delete)
(global-set-key (kbd "C-w") 'text/kill)
(global-set-key (kbd "M-w") 'text/kill-save)
(global-set-key (kbd "C-y") 'text/yank-pop)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)
(global-set-key (kbd "<backtab>") 'text/fold)

;; =====================
;;; Embark
;; =====================
(global-set-key (kbd "C-u") 'embark-act)

;; =====================
;;; Org Agenda Toggle
;; =====================
(defun my/org-agenda-toggle ()
  "Toggle Org Agenda buffer."
  (interactive)
  (if (buffer-live-p (get-buffer "*Org Agenda*"))
      (kill-buffer "*Org Agenda*")
    (org-agenda nil "a")))

(global-set-key (kbd "C-\\") #'my/org-agenda-toggle)
(global-set-key (kbd "C-、") #'my/org-agenda-toggle)  ;; 中文输入法时生效

;; =====================
;;; C-s: Common Commands
;; =====================
(transient-define-prefix transient/c-s ()
  [[:class transient-column "Search & Replace"
           ("s" "search" consult-line)
           ("t" "replace" replace-regexp)]
   [:class transient-column "Buffer & File"
           ("C-d" "delete file" my/delete-current-file)]
   [:class transient-column "Navigation"
           ("o" "outline" (lambda ()
                            (interactive)
                            (if (derived-mode-p 'org-mode)
                                (consult-org-heading)
                              (consult-outline))))
           ("m" "imenu" consult-imenu)
           ("g" "goto line" consult-goto-line)]
   [:class transient-column
           :if (lambda () (bound-and-true-p outline-minor-mode))
           "Outline Command"
           ("TAB" "toggle" text/outline-cycle)
           ("a" "toggle all" text/outline-toggle-all)
           ("h" "hide other" outline-hide-other)
           ("p" "previous visible heading" outline-previous-visible-heading)
           ("n" "next visible heading" outline-next-visible-heading)
           ;;("f" "forward same heading" outline-forward-same-level)
           ;;("b" "backward same heading" outline-backward-same-level)
           ("u" "up heading" outline-up-heading)]])

(global-set-key (kbd "C-s") 'transient/c-s)


;; =====================
;;; C-d: Project Commands
;; =====================
(transient-define-prefix transient/c-d ()
  [[:class transient-column "search"
           ("s" "search" project-find-regexp)
           ("t" "replace" project-query-replace-regexp)
           ("f" "find-grep" find-grep)]
   [:class transient-column "buffer"
           ("k" "kill" project-kill-buffers)]
   [:class transient-column "compile"
           ("c" "compile" project-compile)]
   [:class transient-column "flymake"
           ("p" "previous" flymake-goto-prev-error :transient t)
           ("n" "next" flymake-goto-next-error :transient t)
           ("l" "show line err"
            (lambda ()
              (interactive)
              (consult--forbid-minibuffer)
              (consult--read
               (consult-flymake--candidates
                (flymake-diagnostics (line-beginning-position) (line-end-position)))
               :prompt "Flymake diagnostic: "
               :category 'consult-flymake-error
               :history t
               :require-match t
               :sort nil
               :group (consult--type-group consult-flymake--narrow)
               :narrow (consult--type-narrow consult-flymake--narrow)
               :lookup #'consult--lookup-candidate
               :state (consult--jump-state))))
           ("b" "show buff err" consult-flymake)
           ("e" "show project err" (lambda () (interactive) (consult-flymake t)))]
   [:class transient-column "status"
           ("a" "view all" transient/magit-list-repos)
           ("m" "status" magit-status)]])
(global-set-key (kbd "C-d") 'transient/c-d)


;; =====================
;;; C-r: Tools
;; =====================
(transient-define-prefix transient/c-r ()
  [[:class transient-column "org"
           ("t" "capture" org-capture)
           ("w" "store link" org-store-link)
           ("SPC" "clock out" (lambda ()
                                (interactive)
                                (org-agenda-clock-out)
                                (org-agenda-redo t))
            :if (lambda () (marker-buffer org-clock-marker)))]
   [:class transient-column "window"
           ("m" "save" win/save-window-configuration)
           ("C-m" "restore" win/restore-window-configuration
            :if win/window-configuration-stack)]
   [:class transient-column "snippet"
           ("s" "show all"
            (lambda ()
              (interactive)
              (let ((buff (get-buffer "*YASnippet Tables*")))
                (if (buffer-live-p buff)
                    (kill-buffer buff)
                  (yas-describe-tables)))))
           ("v" "visit" yas-visit-snippet-file)
           ("i" "insert" yas-new-snippet)]
   [:class transient-column "api document"
           ("l" "lookup" devdocs-lookup)]])
(global-set-key (kbd "C-r") 'transient/c-r)

;; =====================
;; Unset Default Keys
;; =====================
(dolist (key '("M-<backspace>" "M-DEL"
               "s-p" "s-n" "s-x" "s-k"
               "C-j" "C-t" "C-z"
               "C-x C-x" "C-x C-e"))
  (global-unset-key (kbd key)))

(provide 'init-global-keybindings)
;;; init-global-keybindings.el ends here
