;;; init-org-agenda-mode.el --- Org-agenda -*- lexical-binding: t -*-
;;; Commentary:
;; Org-agenda configuration for GTD workflow
;;; Code:

(require 'init-gtd-workflow)

;;----------------------------------------
;;; Shortkey
;;----------------------------------------
(use-package org-agenda
  :after org
  :ensure nil
  :bind ( :map org-agenda-mode-map
          ;; define transient menus, keybindings
          ("C-j" . transient/org-agenda-mode)
          ("l" . transient/org-agenda-statistics)
          ;; previous && next
          ("p" . org-agenda-previous-item)
          ("n" . org-agenda-next-item)
          ;; choose agenda view
          ("SPC" . org-agenda/forward-buffer)
          ;; entry show && entry enter
          ("RET" . (lambda()
                     (interactive)
                     (org-agenda-goto)
                     ;; (org-show-entry)
                     (org-narrow-to-subtree)
                     (kill-buffer org-agenda-buffer)))
          ("a" . org-agenda/show-entry-text)
          ;; entry done
          ("d" . org-agenda/archive)
          ;; quit && refresh
          ("q" . (lambda() (interactive)
                   (let ((win (selected-window)))
                     (org-agenda-quit)
                     (delete-window win))))
          ("g" . org-agenda-redo)))


(with-eval-after-load 'org-colview
  (define-key org-columns-map (kbd "n") nil)
  (define-key org-columns-map (kbd "p") nil)
  (define-key org-columns-map (kbd "TAB") nil)
  (define-key org-columns-map (kbd "q") nil))

;;----------------------------------------
;;; Agenda Buffer Display
;;----------------------------------------
(with-eval-after-load 'org-agenda
  (setq org-agenda-window-setup 'current-window
        ;; not show sublevels in agenda
        org-habit-show-habits-only-for-today t
        ;; disable diary
        org-agenda-include-diary nil
        org-agenda-diary-file nil)

  ;; ignore certain tags in agenda
  (setq org-agenda-hide-tags-regexp "ARCHIVE\\|habit")

  ;;;; 'a' Agenda buffer Display
  (setq org-agenda-span 'day
        org-agenda-format-date "%Y-%m-%d %A"
        org-agenda-time-grid '((daily today)
                               ()
                               "" "")
        org-agenda-scheduled-leaders '("Start" "Start %dd")
        org-agenda-deadline-leaders  '("Dead" "In %dd" "Dead %dd"))
  ;; today face
  (set-face-attribute 'org-agenda-date-today nil
                      :weight 'bold
                      :italic nil
                      :underline '(:color foreground-color :style line)
                      :inherit '(org-agenda-date))

  ;;;; Hooks
  ;; Clean empty agenda blocks
  (add-hook 'org-agenda-finalize-hook #'org-agenda/delete-empty-blocks)

  ;; Hook: Org Agenda 'a' not exist, then go to 'n'
  (add-hook 'org-agenda-finalize-hook 'org-agenda/a-hook))

;;----------------------------------------
;;; Agenda buffers Template
;;----------------------------------------
(use-package org-agenda
  :ensure nil
  :init
  (setq org-agenda-custom-commands nil)
  :config
  ;; 'a' Agenda buffer
  (add-to-list 'org-agenda-custom-commands
               '("a" "Agenda"
	         ((agenda ""))
	         ((_ (org-agenda/set-buffer-number 1))
	          (org-agenda-prefix-format "%-10c%?-12t%?-19s")
                  (org-agenda-todo-keyword-format "")
                  (org-agenda-skip-archived-trees nil)
                  (org-agenda-remove-tags t)
	          (org-agenda-skip-scheduled-if-done t)
	          (org-agenda-skip-deadline-if-done  t)
	          (org-deadline-warning-days 2)
	          (org-agenda-sorting-strategy '(time-up deadline-up scheduled-up priority-down))))
               t)

  ;; 'n' Agenda buffer
  (let* ((project-todos
          (mapcar (lambda (file)
                    `(todo "TODO|WAITING"
                           ((org-agenda-overriding-header ,(file-name-base file))
                            (org-agenda-files (list ,file))
                            (org-agenda-skip-function '(org-agenda/skip-entry))
                            (org-agenda-prefix-format "%(org-agenda/prefix-duration)"))))
                  gtd/projects)))
    (add-to-list 'org-agenda-custom-commands
                 `("n" "Next Step"
                   ((todo "TODO|WAITING"
                          ((org-agenda-overriding-header "TODO & WAITING")
                           (org-agenda-files (list gtd/task))
                           (org-agenda-prefix-format "%(org-agenda/prefix-duration)")))
                    ,@project-todos
                    (todo "PROJECT"
                          ((org-agenda-overriding-header "STUCK PROJECT")
                           (org-agenda-files (list gtd/task))
                           (org-agenda-prefix-format "")
                           (org-agenda-skip-function
                            '(org-agenda-skip-subtree-if 'todo '("TODO" "WAITING"))))))
                   ((_ (org-agenda/set-buffer-number 2))
                    (org-agenda-todo-keyword-format "")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-agenda-todo-ignore-scheduled 'all)
                    (org-agenda-todo-ignore-deadlines 'all)
                    (org-agenda-todo-ignore-timestamp 'all)
                    (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))))

  ;; 'i' Agenda buffer
  (add-to-list 'org-agenda-custom-commands
               '("i" "inbox"
	         ((tags "LEVEL=1"))
	         ((_ (org-agenda/set-buffer-number 3))
	          (org-agenda-overriding-header "Inbox")
	          (org-agenda-prefix-format "%(org-agenda/prefix-duration)")
	          (org-agenda-files (list gtd/inbox))
	          (org-agenda-sorting-strategy '(priority-down alpha-up effort-up))))
               t)

  ;; 'p' Agenda buffer
  (add-to-list 'org-agenda-custom-commands
               '("p" "project"
	         ((tags "LEVEL=1/PROJECT"))
	         ((org-agenda-overriding-header "PROJECT ING")
	          (org-agenda-prefix-format "%-10c")
	          (org-agenda-todo-keyword-format "")
	          (org-overriding-columns-format "%24ITEM %10CATEGORY %1PRIORITY %Effort %10CLOCKSUM")
	          (org-agenda-sorting-strategy '(category-keep))))
               t)

  ;; 'r' Agenda buffer
  (add-to-list 'org-agenda-custom-commands
               '("r" "archive"
	         ((tags "LEVEL=2"
		        ((org-agenda-overriding-header "PROJECT")
                         (org-agenda-skip-function '(org-agenda/skip-if-not-root-entry "Project"))))
                  (tags "LEVEL=2"
		        ((org-agenda-overriding-header "DONE && CANCEL")
                         (org-agenda-skip-function '(org-agenda/skip-if-not-root-entry "Todo && Waiting"))))
	          (search "Interrupt"
		          ((org-agenda-overriding-header "Interrupt")
                           (org-agenda-skip-archived-trees nil)
		           (org-tags-match-list-sublevels nil))))
	         ((org-agenda-sorting-strategy '(category-keep))
	          (org-agenda-files (list gtd/archive))
	          (org-agenda-prefix-format "")
	          (org-agenda-todo-keyword-format "")
	          (org-overriding-columns-format "%24ITEM %1PRIORITY %10TAGS %Effort %10CLOCKSUM %22ARCHIVE_TIME")))
               t))

;;----------------------------------------
;;; Org columns
;;----------------------------------------
(with-eval-after-load 'org-colview
  (setq org-columns-default-format "%24ITEM %7TODO %1PRIORITY %10TAGS %Effort %10CLOCKSUM")
  (set-face-attribute 'org-column nil
                      :background (face-attribute 'default :background)
                      :height     (face-attribute 'default :height)
                      :family     (face-attribute 'default :family)))

;;----------------------------------------
;;; Save all Org buffers on exit
;;----------------------------------------
;; TODONOW not work
(with-eval-after-load 'org
  (add-hook 'kill-emacs-hook 'org-save-all-org-buffers))

(provide 'init-org-agenda-mode)
;;; init-org-agenda-mode.el ends here
