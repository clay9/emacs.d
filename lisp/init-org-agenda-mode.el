;;; init-org-agenda-mode.el --- Org-agenda -*- lexical-binding: t -*-
;;; Commentary:
;; Org-agenda configuration for GTD workflow
;;; Code:

(require 'init-gtd-workflow)

(require 'sub-org-agenda-mode-keybindings)

;;----------------------------------------
;;; Agenda Buffer Display
;;----------------------------------------
(with-eval-after-load 'org-agenda
  (setq org-agenda-window-setup 'only-window
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
        org-agenda-scheduled-leaders '("" "Start %dd")
        org-agenda-deadline-leaders  '("❗" "⏳ %dd" "🔥 %dd"))
  ;; today face
  (set-face-attribute 'org-agenda-date-today nil
                      :weight 'bold
                      :italic nil
                      :underline '(:color foreground-color :style line)
                      :inherit '(org-agenda-date))

  ;;;; Hooks
  ;; Hook: Clean empty agenda blocks
  (defun org-agenda/delete-empty-blocks ()
    "Remove empty agenda blocks.
  A block is identified as empty if there are fewer than 2
  non-empty lines in the block (excluding the line with
  `org-agenda-block-separator' characters)."
    (when org-agenda-compact-blocks
      (user-error "Cannot delete empty compact blocks"))
    (setq buffer-read-only nil)
    (save-excursion
      (goto-char (point-min))
      (let* ((blank-line-re "^\\s-*$")
             (content-line-count (if (looking-at-p blank-line-re) 0 1))
             (start-pos (point))
             (block-re (format "%c\\{10,\\}" org-agenda-block-separator)))
        (while (and (not (eobp)) (forward-line))
          (cond
           ((looking-at-p block-re)
            (when (< content-line-count 2)
              (delete-region start-pos (1+ (point-at-bol))))
            (setq start-pos (point))
            (forward-line)
            (setq content-line-count (if (looking-at-p blank-line-re) 0 1)))
           ((not (looking-at-p blank-line-re))
            (setq content-line-count (1+ content-line-count)))))
        (when (< content-line-count 2)
          (delete-region start-pos (point-max)))
        (goto-char (point-min))
        ;; The above strategy can leave a separator line at the beginning
        ;; of the buffer.
        (when (looking-at-p block-re)
          (delete-region (point) (1+ (point-at-eol))))))
    (setq buffer-read-only t))
  (add-hook 'org-agenda-finalize-hook #'org-agenda/delete-empty-blocks)

  ;; Hook: Org Agenda 'a' not exist, then go to 'n'
  (defun org-agenda/a-hook ()
    (when (= org-agenda/current-buffer-number 1)
      (unless (my/org-agenda-day-view-has-items)
        (org-agenda nil "n")))))

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
	          (org-agenda-prefix-format "%-13t%-20s")
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
                            (org-agenda-skip-function
                             (lambda ()
                               (or (org-agenda/skip-entry)
                                   (org-agenda-skip-entry-if 'nottodo '("TODO" "WAITING")))))
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
;;; Functions for Agenda buffers Template
;;----------------------------------------
(defun org-agenda/skip-entry ()
  "Skip entry if its root heading TODO state is DONE or CANCEL."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (org-entry-end-position)))
      (while (and (org-up-heading-safe) (> (org-outline-level) 1)))
      (if (org-entry-is-done-p)
          end
        nil))))
(defun org-agenda/skip-if-not-root-entry (root)
  (let* ((end (org-entry-end-position)))
    (while (and (org-up-heading-safe) (> (org-outline-level) 1)))
    (message "%s: %s" root (nth 4 (org-heading-components)))
    (if (string= root (nth 4 (org-heading-components)))
        nil
      end)))

(defun org-agenda/prefix-duration ()
  "Return a prefix string for Org Agenda.
Shows time duration since CAPTURE_TIME and top-level heading title."
  (let* ((capture-time (org-entry-get nil "CAPTURE_TIME"))
         (v1 "")
         (v2 "")
         (todo (org-get-todo-state)))
    (when capture-time
      (let* ((dura (org-time-since capture-time))
             (days (car dura))
             (seconds (cadr dura))
             (hours (/ seconds 3600))
             (minutes (/ seconds 60)))
        (cond
         ((> days 0) (setq v1 (format "%dd" days)))
         ((> hours 0) (setq v1 (format "%dh" hours)))
         ((> minutes 0) (setq v1 (format "%dm" minutes))))))
    (when (string= todo "WAITING")
      (setq v2 "wait"))
    (format "%-6s%-9s" v1 v2)))

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
  ;; 每次 Emacs 空闲 3 分钟执行一次
  (run-with-idle-timer 180 t #'my/maybe-org-agenda-a))


(provide 'init-org-agenda-mode)
;;; init-org-agenda-mode.el ends here
