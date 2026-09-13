;;; init-gtd-basic.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------
;;; GTD files
;;----------------------------------------
(use-package org-agenda
  :after org
  :ensure nil
  :config
  (defvar gtd/dir "~/my/gtd/")
  (defvar gtd/inbox   (expand-file-name "gtd_common/inbox.org"   gtd/dir))
  (defvar gtd/task    (expand-file-name "gtd_common/task.org"    gtd/dir))
  (defvar gtd/archive (expand-file-name "gtd_common/archive.org" gtd/dir))

  (setq org-agenda-files
        (when (file-directory-p gtd/dir)
          (directory-files gtd/dir t "^gtd")))

  (setq gtd/projects
        (when (file-directory-p (expand-file-name "gtd" gtd/dir))
          (directory-files (expand-file-name "gtd" gtd/dir) t "\\.org$"))))

;;----------------------------------------
;;; TODO keywords and priority
;;----------------------------------------
(with-eval-after-load 'org
  (setq org-todo-keywords
        '((type "TODO(t)" "WAITING(w)" "PROJECT(p)" "|" "DONE(d)" "CANC(c)")))

  (setq org-enforce-todo-dependencies t
        org-closed-keep-when-no-todo nil
        org-use-fast-todo-selection 'expert)

  ;; priorities
  (setq org-highest-priority ?A
        org-lowest-priority  ?D
        org-default-priority ?D)

  ;; set caputre_time when set todo-state
  (defun my/org-set-effort-and-time ()
    "在 TODO 状态变更时，没有 CAPTURE_TIME 时设置属性。"
    (let ((v (org-entry-properties nil "EFFORT"))
          (todo-key org-state)
          (time-string (format-time-string
                        "[%Y-%m-%d %H:%M]"
                        (time-add (current-time) (days-to-time 0)))))
      (when (member todo-key '("TODO" "WAITING" "PROJECT"))
        ;; set capture time
        (when (not (org-entry-get nil "CAPTURE_TIME"))
          (org-set-property "CAPTURE_TIME" time-string))
        ;; set effort
        (when (or (not v)
                  (string= (cdr (car v)) "0:00"))
          (org-set-effort)))))
  (add-hook 'org-after-todo-state-change-hook #'my/org-set-effort-and-time)

  (setq org-priority-faces
        '((?A . (:foreground "#ff6b6b" :weight medium))
          (?B . (:foreground "#f4a261" :weight medium))
          (?C . (:foreground "#e9c46a" :weight medium))
          (?D . (:foreground "#8fa3bf" :weight medium)))))

;;----------------------------------------
;;; Clock and effort
;;----------------------------------------
(with-eval-after-load 'org
  ;; global properties
  (setq org-global-properties
        '(("Effort_ALL" . "0:00 0:15 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")))

  ;; Save clock data and state changes in LOGBOOK
  (setq org-clock-into-drawer t
        org-log-into-drawer t
        org-clock-out-remove-zero-time-clocks t)

  ;; Set effort automatically on clock in
  (defun my/set-effort-when-clock-in ()
    (let ((v (org-entry-properties nil "EFFORT"))
          (todo-key (org-get-todo-state)))
      (when (and (not (string= todo-key "DONE"))
                 (or (not v)
                     (string= (cdr (car v)) "0:00")))
        (org-set-effort))))
  (add-hook 'org-clock-in-hook #'my/set-effort-when-clock-in)

  ;; Show/hide clock display
  (require 'sub-gtd-clock-display))

(with-eval-after-load 'org-agenda
  ;; skip zero-time in clock-report
  (setq org-agenda-clockreport-parameter-plist
        '(:stepskip0 t :link t :maxlevel 3 :fileskip0 t)))


(provide 'init-gtd-basic)
;;; init-gtd-basic.el ends here
