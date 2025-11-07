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
(global-set-key (kbd "C-\\") #'my/org-agenda-toggle)

;; =====================
;;; C-s: Common Commands
;; =====================
(transient-define-prefix transient/c-s ()
  [[:class transient-column "Search"
           ("s" "Search" consult-line)
           ("t" "Replace" replace-regexp)]
   [:class transient-column
           :if (lambda () (buffer-file-name))
           "File"
           ("C-d" "Delete" my/delete-current-file)]
   [:class transient-column "Navigation"
           ("o" "Outline" (lambda ()
                            (interactive)
                            (if (derived-mode-p 'org-mode)
                                (consult-org-heading)
                              (consult-outline))))
           ("m" "Imenu" consult-imenu)
           ("g" "Goto line" consult-goto-line)]
   [:class transient-column
           :if (lambda () (bound-and-true-p outline-minor-mode))
           "Outline Command"
           ("TAB" "toggle" text/outline-cycle)
           ("a" "toggle all" text/outline-toggle-all)
           ("h" "hide other" outline-hide-other)
           ;;("p" "previous visible heading" outline-previous-visible-heading :transient t)
           ;;("n" "next visible heading" outline-next-visible-heading :transient t)
           ;;("f" "forward same heading" outline-forward-same-level)
           ;;("b" "backward same heading" outline-backward-same-level)
           ("u" "up heading" outline-up-heading)]])

(global-set-key (kbd "C-s") 'transient/c-s)

;; =====================
;;; C-d: Project Commands
;; =====================
(transient-define-prefix transient/c-d ()
  [[:class transient-column "Search"
           ("s" "Minibuf" consult-ripgrep)
           ("t" "Buffer" my/rg-project)]
   [:class transient-column "Buffer"
           ("k" "Kill" project-kill-buffers)]
   [:class transient-column "Compile"
           ("c" "Compile" project-compile)]
   [:class transient-column
           :if (lambda () (bound-and-true-p flymake-mode))
           "Flymake"
           ;;("p" "previous" flymake-goto-prev-error :transient t)
           ;;("n" "next" flymake-goto-next-error :transient t)
           ("l" "Show line error"
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
           ("b" "Show buff error" consult-flymake)
           ("p" "Show project error" (lambda () (interactive) (consult-flymake t)))]
   [:class transient-column "Magit"
           ("m" "Status" magit-status)]])
(global-set-key (kbd "C-d") 'transient/c-d)


;; =====================
;;; C-r: Tools
;; =====================
(transient-define-prefix transient/c-r ()
  [[:class transient-column "Capture"
           ("h" "capture" org-capture)
           ("j" "journal" org-journal-new-entry)
           ("w" "store link" org-store-link)]
   [:class transient-column
           :if (lambda () (marker-buffer org-clock-marker))
           "Clock"
           ("SPC" "clock out" org-agenda-clock-out)]
   [:class transient-column "Window"
           ("m" "save" win/save-window-configuration)
           ("C-m" "restore" win/restore-window-configuration
            :if win/window-configuration-stack)]
   [:class transient-column "Snippet"
           ("v" "visit" yas-visit-snippet-file)
           ("i" "insert" yas-new-snippet)]
   ;; [:class transient-column "api document"
   ;;         ("l" "lookup" devdocs-lookup)]
   ])
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
