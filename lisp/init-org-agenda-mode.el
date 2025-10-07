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

(use-package org-columns
  :after org
  :ensure nil
  :bind ( :map org-columns-map
          ("p" . org-agenda-previous-item)
          ("n" . org-agenda-next-item)))

;;----------------------------------------
;;; Agenda Buffer Display
;;----------------------------------------
(with-eval-after-load 'org-agenda
  (setq org-agenda-window-setup 'current-window
        ;; not show sublevels in agenda
        org-habit-show-habits-only-for-today t
        ;; diary
        org-agenda-include-diary nil
        org-agenda-diary-file my/file-diary)

  ;; ignore certain tags in agenda
  (setq org-agenda-hide-tags-regexp "emacs\\|org\\|ccIDE\\|qygame\\|habit")

  ;; default span
  (setq org-agenda-span 'day)
  ;; time grid
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 1200 1400 1800 1900 2300)
          "......" "----------------"))
  ;; today face
  (set-face-attribute 'org-agenda-date-today nil
                      :weight 'bold
                      :italic nil
                      :underline '(:color foreground-color :style line)
                      :inherit '(org-agenda-date))
  ;; schedule/deadline leader strings
  (setq org-agenda-scheduled-leaders '("Start" "Start %dd")
        org-agenda-deadline-leaders  '("Dead" "In %dd" "Dead %dd"))

  ;; Clean empty agenda blocks
  (add-hook 'org-agenda-finalize-hook #'org-agenda/delete-empty-blocks)

  ;; Hook: Org Agenda 'a' not exist, then go to 'n'
  (add-hook 'org-agenda-finalize-hook 'org-agenda/a-hook))

;;----------------------------------------
;;; Agenda buffers Template
;;----------------------------------------
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("a" "Agenda"
	   ((agenda ""))
	   ((_ (org-agenda/set-buffer-number 1))
	    (org-agenda-prefix-format "%-10c%?-12t%?-19s")
            (org-agenda-todo-keyword-format "")
            (org-agenda-remove-tags t)
	    (org-agenda-skip-scheduled-if-done t)
	    (org-agenda-skip-deadline-if-done  t)
            (org-agenda-archives-mode t)
	    (org-deadline-warning-days 2)
	    (org-agenda-sorting-strategy '(time-up todo-state-up scheduled-up deadline-up priority-up))))
	  ("n" "Next Step"
	   ((todo "TODO"
		  ((org-agenda-overriding-header "TODO")
                   (org-agenda-files (list my/file-task))
                   (org-agenda-skip-function `(org-agenda/skip-entry))
                   (org-agenda-prefix-format "%(my/org-agenda-pf-next)")))
	    (todo "WAITING"
		  ((org-agenda-overriding-header "WAITING")
                   (org-agenda-files (list my/file-task))
                   (org-agenda-prefix-format "%(my/org-agenda-pf-next)")))
            (todo "TODO|WAITING"
		  ((org-agenda-overriding-header "emacs")
                   (org-agenda-files (list (expand-file-name "gtd/emacs.org" my/gtd-dir)))
		   (org-agenda-skip-function `(org-agenda/skip-entry))
                   (org-agenda-prefix-format "%(my/org-agenda-pf-project)")))
            (todo "TODO|WAITING"
		  ((org-agenda-overriding-header "qygame")
                   (org-agenda-files (list (expand-file-name "gtd/qygame.org" my/gtd-dir)))
		   (org-agenda-skip-function `(org-agenda/skip-entry))
                   (org-agenda-prefix-format "%(my/org-agenda-pf-project)")))
            ;; (todo "TODO|WAITING"
	    ;;       ((org-agenda-overriding-header "PROJECT yygame")
            ;;        (org-agenda-files (list (expand-file-name "gtd/yygame.org" my/gtd-dir)))
	    ;;        (org-agenda-skip-function `(org-agenda/skip-entry))
            ;;        (org-agenda-prefix-format "%(my/org-agenda-pf-project)")))
	    (todo "PROJECT"
	          ((org-agenda-overriding-header "STUCK PROJECT")
                   (org-agenda-files gtd/common-files)
	           (org-agenda-prefix-format "%(my/org-agenda-pf-next-p)")
	           (org-agenda-skip-function `(org-agenda-skip-subtree-if 'todo '("TODO" "WAITING"))))))
	   ((_ (org-agenda/set-buffer-number 2))
            (org-agenda-todo-keyword-format "")
	    (org-agenda-tags-todo-honor-ignore-options t)
	    (org-agenda-todo-ignore-scheduled 'all)
	    (org-agenda-todo-ignore-deadlines 'all)
	    (org-agenda-todo-ignore-timestamp 'all)
	    (org-agenda-sorting-strategy '(priority-down effort-up))))
	  ("i" "inbox"
	   ((tags "LEVEL=1"))
	   ((_ (org-agenda/set-buffer-number 3))
	    (org-agenda-overriding-header "Inbox")
	    (org-agenda-prefix-format "%(my/org-agenda-pf-next)")
	    (org-agenda-files (list my/file-inbox))
	    (org-agenda-sorting-strategy '(priority-down alpha-up effort-up))))
	  ("p" "project"
	   ((tags "LEVEL=1/PROJECT"
                  ((org-agenda-files (list my/file-task))))
            (tags "LEVEL=1"
	          ((org-agenda-files gtd/no-common-files))))
	   ((_ (org-agenda/set-buffer-number 4))
            (org-agenda-overriding-header "PROJECT")
	    (org-agenda-prefix-format "%-10c")
	    (org-agenda-todo-keyword-format "")
	    (org-overriding-columns-format "%24ITEM %10CATEGORY %1PRIORITY %Effort %10CLOCKSUM")
	    (org-agenda-sorting-strategy '(category-keep))))
	  ("r" "archive"
	   ((tags "TODO={PROJECT}"
		  ((org-agenda-overriding-header "Archive PROJECT")))
            (tags "TODO={TODO}"
		  ((org-agenda-overriding-header "Archive DONE && CANCEL")))
	    (tags "TODO={DONE}"
		  ((org-agenda-overriding-header "Archive Interrupt")
		   (org-tags-match-list-sublevels nil) )))
	   ((_ (org-agenda/set-buffer-number 5))
	    (org-agenda-sorting-strategy '(category-keep))
	    (org-agenda-files (list my/file-archive))
	    (org-agenda-prefix-format "")
	    (org-agenda-todo-keyword-format "")
	    (org-overriding-columns-format "%24ITEM %1PRIORITY %10TAGS %Effort %10CLOCKSUM %22ARCHIVE_TIME"))))))

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
