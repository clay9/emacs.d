;;; init-global-shorkey.el --- global key  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; window
(global-set-key (kbd "C-x o") 'my/switch-window)
(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)
(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))
(global-set-key (kbd "C-x t") 'sanityinc/toggle-current-window-dedication)

;; buffer
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x C-b") 'consult-project-buffer)
(global-set-key (kbd "C-x a") 'beginning-of-buffer)
(global-set-key (kbd "C-x e") 'end-of-buffer)
(global-set-key (kbd "C-x s") 'save-buffer)
(global-set-key (kbd "M-k") 'kill-current-buffer)

;; file
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x C-f") 'project-find-file)

;; delete | kill | yank | select
(global-set-key (kbd "C-<backspace>") 'my/delete)
(global-set-key (kbd "C-k") 'my/delete)
(global-set-key (kbd "C-w") 'my/kill)
(global-set-key (kbd "M-w") 'my/kill-save)
(global-set-key (kbd "C-y") 'my/yank-pop)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; action
;;(global-set-key (kbd "C-SPC") 'embark-act)
(global-set-key (kbd "C-u") 'embark-act) ;; 适应C-u

;; org-mode
(global-set-key (kbd "C-\\") #'(lambda() (interactive)
                                 (if (buffer-live-p (get-buffer "*Org Agenda*"))
                                     (kill-buffer org-agenda-buffer-name)
                                   (add-hook 'org-agenda-finalize-hook 'my/org-agenda-empty-p)
                                   (org-agenda nil "a"))))
(global-set-key (kbd "C-、") #'(lambda() (interactive)
                                 (if (buffer-live-p (get-buffer "*Org Agenda*"))
                                     (kill-buffer org-agenda-buffer-name)
                                   (add-hook 'org-agenda-finalize-hook 'my/org-agenda-empty-p)
                                   (org-agenda nil "a")))) ;;make it work when in 中文输入法


;; C-s:
(transient-define-prefix transient/c-s()
  "Common comands."
  [[:class transient-column "search"
           ("s" "search" consult-line)
           ("t" "replace" replace-string)]
   [:class transient-column "buffer"
           ("TAB" "indent" (lambda() (interactive)
                             (indent-region (point-min) (point-max))))
           ("C-d" "del file" my/delete-current-file)]
   [:class transient-column "navigate"
           ("m" "imenu" consult-imenu)
           ("o" "outline" consult-outline)
           ("g" "go line" consult-goto-line)]])
(global-set-key (kbd "C-s") 'transient/c-s)



;; C-d: project
(transient-define-prefix transient/c-d()
  "Project comands."
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
           ("l" "show line err" (lambda()
                                  (interactive)
                                  (consult--forbid-minibuffer)
                                  (consult--read
                                   (consult-flymake--candidates
                                    (flymake-diagnostics (line-beginning-position) (line-end-position)))
                                   :prompt "Flymake diagnostic: "
                                   :category 'consult-flymake-error
                                   :history t ;; disable history
                                   :require-match t
                                   :sort nil
                                   :group (consult--type-group consult-flymake--narrow)
                                   :narrow (consult--type-narrow consult-flymake--narrow)
                                   :lookup #'consult--lookup-candidate
                                   :state (consult--jump-state))))
           ;;flymake-show-buffer-diagnostics
           ("b" "show buff err" consult-flymake)
           ;;flymake-show-project-diagnostics
           ("e" "show project err" (lambda() (interactive )(consult-flymake t)))]
   [:class transient-column "status"
           ("a" "view all" transient/magit-list-repos)
           ("m" "status" magit-status)]])
(global-set-key (kbd "C-d") 'transient/c-d)


;; C-r: tools
(transient-define-prefix transient/c-r()
  "Common comands."
  [[:class transient-column "org"
           ("r" "capture" org-capture)
           ("w" "store link" org-store-link)
           ("SPC" "clock out" (lambda() (interactive)
        		        (org-agenda-clock-out)
        		        (my/org-agenda-redo))
            :if (lambda() (interactive) (marker-buffer org-clock-marker)))]
   [:class transient-column "window"
           ("m" "save" transient/c-s/save-window)
           ("C-m" "restore" transient/c-s/restore-window
            :if transient/c-r/is-window-empty)]
   [:class transient-column "snippet"
           ("s" "show all" (lambda() (interactive)
                             (let ((buff (get-buffer "*YASnippet Tables*")))
                               (if (buffer-live-p buff)
                                   (kill-buffer buff)
                                 (yas-describe-tables)))))
           ("v" "visit" yas-visit-snippet-file)
           ("i" "insert" yas-new-snippet)]
   [:class transient-column "api document"
	   ("l" "lookup" devdocs-lookup)]])
(global-set-key (kbd "C-r") 'transient/c-r)


;; unset
(global-set-key (kbd "M-<backspace>") 'nil)
(global-set-key (kbd "M-DEL") 'nil)

(global-set-key (kbd "s-p") 'nil)
(global-set-key (kbd "s-n") 'nil)
(global-set-key (kbd "s-x") 'nil)
(global-set-key (kbd "s-k") 'nil)

(global-set-key (kbd "C-j") 'nil) ;;for major-mode
(global-set-key (kbd "C-t") 'nil)
(global-set-key (kbd "C-z") 'nil)

(global-set-key (kbd "C-x C-x") 'nil)
(global-set-key (kbd "C-x C-e") 'nil)

(provide 'init-global-shortkey)
;;; init-global-shortkey.el ends here
