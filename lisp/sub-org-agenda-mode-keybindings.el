;;; sub-org-agenda-mode-keybindings.el --- Org Agenda Keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------
;;; KeyBindings
;;----------------------------------------
(use-package org-agenda
  :after org
  :ensure nil
  :bind ( :map org-agenda-mode-map
          ;; define transient menus, keybindings
          ("?" . transient/org-agenda-mode)
          ("a" . transient/org-agenda-add-info)
          ("t" . transient/org-agenda-schedule-deadline)
          ("c" . transient/org-agenda-clock)
          ("v" . transient/org-agenda-view)
          ("F" . transient/org-agenda-filter)
          ("S" . transient/org-agenda-statistics)
          ;; previous && next
          ("p" . org-agenda-previous-item)
          ("n" . org-agenda-next-item)
          ;; log mode
          ("l" . my/org-agenda-log-mode)
          ;; clock report
          ("r" . org-agenda-clockreport-mode)
          ;; archive
          ("d" . org-agenda/archive)
          ;; choose agenda view
          ("SPC" . org-agenda/forward-buffer)
          ;; entry show && entry enter
          ([return] . (lambda() (interactive)
                        (org-agenda-goto)
                        (org-narrow-to-subtree)
                        (org-cycle 1)))
          ([tab] . org-agenda/show-entry-text)
          ;; quit
          ("q" . (lambda() (interactive)
                   (org-agenda-quit)
                   (win/restore-window-configuration)))))

(with-eval-after-load 'org-colview
  (define-key org-columns-map (kbd "n") nil)
  (define-key org-columns-map (kbd "p") nil)
  (define-key org-columns-map (kbd "TAB") nil)
  (define-key org-columns-map (kbd "q") nil))

;;----------------------------------------
;;; Transient menus
;;----------------------------------------
(use-package org-agenda
  :after org
  :ensure nil
  :config
  (transient-define-prefix transient/org-agenda-mode()
    ["Transient and dwim commands"
     ("a" "Add Info" transient/org-agenda-add-info)
     ("t" "Schedule & Deadline" transient/org-agenda-schedule-deadline)
     ("c" "Clock" transient/org-agenda-clock)
     ("v" "View" transient/org-agenda-view :if (lambda() (org-agenda-check-type nil 'agenda)) :transient t)
     ("F" "Filter" transient/org-agenda-filter)
     ("S" "Statistics" transient/org-agenda-statistics)])

  (transient-define-prefix transient/org-agenda-add-info()
    ["Add Info"
     ("t" "todo" org-agenda-todo)
     (":" "tag" org-agenda-set-tags)
     ("-" "-priority" org-agenda-priority-down)
     ("=" "+priority" org-agenda-priority-up)
     ("p" "property set" org-agenda-set-property)
     ("e" "effort" org-agenda-set-effort)
     ("d" "archive done" org-agenda/archive)])
  (transient-define-prefix transient/org-agenda-schedule-deadline()
    ["TimeStamp"
     ("s" "schedule" org-agenda-schedule)
     ("d" "deadline" org-agenda-deadline)])
  (transient-define-prefix transient/org-agenda-clock()
    ["clock"
     ("i" "clock in" org-agenda-clock-in)
     ("o" "clock out" org-agenda-clock-out)
     ("c" "clock cancel" org-agenda-clock-cancel)
     ("g" "clock go" org-agenda-clock-goto)])
  (transient-define-prefix transient/org-agenda-view()
    [["view"
      ("d" "day" (lambda() (interactive) (org-agenda/a-action "day" nil) (org-agenda-goto-today)))
      ("w" "week" (lambda() (interactive) (org-agenda/a-action "week" nil)))
      ("W" "fortnight" (lambda() (interactive) (org-agenda/a-action "fortnight" nil)))
      ("m" "month" (lambda() (interactive) (org-agenda/a-action "month" nil)))
      ("y" "year" (lambda() (interactive) (org-agenda/a-action "year" nil)))]
     ["nav"
      ("g" "go data" org-agenda-goto-date)
      ("b" "previous" org-agenda-earlier)
      ("f" "next" org-agenda-later)]])
  (transient-define-prefix transient/org-agenda-filter()
    [["filter"
      ("t" "tags" org-search-view)
      ("s" "search" org-tags-view)
      ("<backspace>" "clear" org-agenda-filter-remove-all)]
     ["quick choose"
      ("e" "effort < 15min" (lambda() (interactive)
		              (org-agenda-filter-remove-all)
		              ;; must set this val. or `r' will remove all filter
		              (setq org-agenda-effort-filter (list "+<0:15"))
		              (org-agenda-filter-apply org-agenda-effort-filter 'effort)))]])
  (transient-define-prefix transient/org-agenda-statistics()
    [["project"
      ("p" "project" (lambda() (interactive)
                       (org-agenda nil "p")
                       (org-agenda-columns)
                       (org-agenda-next-item 1)))
      ("a" "archive" (lambda() (interactive)
                       (org-agenda nil "r")
                       (org-agenda-columns)
                       (org-agenda-next-item 1)))]])

  (defun org-agenda/a-action (period mode)
    (interactive)
    (unless (org-agenda-check-type nil 'agenda)
      (org-agenda nil "a"))

    (pcase period
      ("day" (unless (eq org-agenda-span 'day) (org-agenda-day-view)))
      ("week" (unless (eq org-agenda-span 'week) (org-agenda-week-view)))
      ("month" (unless (eq org-agenda-span 'month) (org-agenda-month-view)))
      ("year" (unless (eq org-agenda-span 'year) (org-agenda-year-view)))
      ("fortnight" (unless (eq org-agenda-span 'fortnight) (org-agenda-fortnight-view))))

    (pcase mode
      ("report" (unless (bound-and-true-p org-agenda-clockreport-mode)
                  (org-agenda-clockreport-mode)))
      ("log" (org-agenda-log-mode 'clockcheck))))

  (defun org-agenda/archive ()
    (interactive)
    (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
		         (org-agenda-error)))
	   (buffer (marker-buffer hdmarker))
	   (pos (marker-position hdmarker))
	   (inhibit-read-only t)
           (todo-state))
      ;; goto org buffer
      (org-with-remote-undo buffer
        (with-current-buffer buffer
	  (widen)
	  (goto-char pos)

          (setq todo-state (org-get-todo-state))

          ;; 1. check todo-state
          (while (not (org-entry-is-done-p))
            (org-todo))
          ;; 2. close clock if in this item
          (when (and (org-clock-is-active)
                     (string= (org-entry-get nil "ITEM") org-clock-current-task))
            (org-agenda-clock-out))
          ;; 3. move to %project%_archive or archive.org::
          (if (string= "task.org" (buffer-name buffer))
              (let* ((org-archive-location (if (string= todo-state "PROJECT")
                                               "archive.org::* Project"
                                             "archive.org::* Todo && Waiting")))
                (org-archive-subtree))
            (org-archive-subtree)))))
    ;; revert org-agenda buff
    (org-agenda-redo t))

  ;; show/hide entry text
  (defun org-agenda/show-entry-text()
    (interactive)
    (save-excursion
      (let ((org-agenda-entry-text-maxlines 100)
            (org-agenda-entry-text-leaders nil)
            (show-p (org-agenda/hide-entry-text)))
        (when (and show-p
                   (org-get-at-bol 'org-hd-marker))
          (org-agenda-entry-text-show-here)))))
  (defun org-agenda/hide-entry-text ()
    "Remove current shown entry context."
    (let ((res t))
      (mapc (lambda (o)
	      (when (eq (overlay-get o 'org-overlay-type)
		        'agenda-entry-content)
                (setq res nil)
	        (delete-overlay o)))
            (overlays-at (point)))
      res)))

;;----------------------------------------
;;; toggle org agenda log mode
;;----------------------------------------
(defun my/org-agenda-log-mode()
  "log mode only show log items"
  (interactive)
  (if org-agenda-show-log
      (org-agenda-log-mode)
    (org-agenda-log-mode '(16))))

;;----------------------------------------
;;; toggle org agenda buffer
;;----------------------------------------
(defun my/org-agenda-toggle ()
  "Toggle Org Agenda buffer."
  (interactive)
  (if (buffer-live-p (get-buffer "*Org Agenda*"))
      (progn
        (kill-buffer "*Org Agenda*")
        (win/restore-window-configuration))
    (win/save-window-configuration)
    (my/org-agenda-a)))

;;----------------------------------------
;;; 快速切换agenda buffer
;;----------------------------------------
(defvar org-agenda/current-buffer-number 1)
(defun org-agenda/set-buffer-number(number)
  (setq org-agenda/current-buffer-number number))

(defun my/org-agenda-a ()
  (let ((hook 'org-agenda/a-hook))
    (unwind-protect
        (progn
          (add-hook 'org-agenda-finalize-hook hook)
          (org-agenda nil "a"))
      (remove-hook 'org-agenda-finalize-hook hook))))

(defun org-agenda/forward-buffer ()
  "agenda-view, next-view, inbox-view"
  (interactive)
  (unless (derived-mode-p 'org-agenda-mode)
    (user-error "Can only append from within agenda buffer"))
  (let ((org-agenda-multi nil))
    (pcase org-agenda/current-buffer-number
      (1 (org-agenda nil "n"))
      (2 (let ((inbox
                (with-current-buffer (find-buffer-visiting gtd/inbox)
                  (goto-char (point-min))
                  (re-search-forward "* " nil t))))
           (if inbox
	       (org-agenda nil "i")
             (my/org-agenda-a))))
      (3 (my/org-agenda-a)))))

;;----------------------------------------
;;; check org-agenda day view has items
;;----------------------------------------
(use-package org-agenda
  :ensure nil
  :config
  (defun my/org-agenda-day-view-has-items ()
    (if (buffer-live-p (get-buffer "*Org Agenda*"))
        (with-current-buffer org-agenda-buffer
          (if (next-single-property-change (line-end-position) 'org-marker)
              t
            nil))
      nil)))

(provide 'sub-org-agenda-mode-keybindings)
;;; sub-org-agenda-mode-keybindings.el ends here
