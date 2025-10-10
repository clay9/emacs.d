;;; init-gtd-workflow-fun.el --- GTD workflow by Org Agenda -*- lexical-binding: t -*-
;;; Commentary:
;; Org-agenda configuration for GTD workflow
;;; Code:

(require 'fun-gtd-workflow)

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
        '((type "TODO(t)" "WAITING(w)" "PROJECT(p)" "|" "DONE(d)" "CANCEL(c)")))

  (setq org-enforce-todo-dependencies t
        org-closed-keep-when-no-todo nil
        org-use-fast-todo-selection 'expert)

  ;; priorities
  (setq org-highest-priority ?A
        org-lowest-priority  ?D
        org-default-priority ?D)

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

  ;; Show/hide clock in header line
  (defun my/show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))
  (defun my/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))
  (add-hook 'org-clock-in-hook #'my/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook #'my/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook #'my/hide-org-clock-from-header-line))

(with-eval-after-load 'org-agenda
  ;; clock face
  (set-face-attribute 'org-agenda-clocking nil :extend t :background "color-29")
  ;; skip zero-time in clock-report
  (setq org-agenda-clockreport-parameter-plist
        '(:stepskip0 t :link t :maxlevel 3 :fileskip0 t)))

;;----------------------------------------
;;; Capture, refile, archive
;;----------------------------------------
(use-package org-capture
  :ensure nil
  :init
  (setq org-capture-templates nil)
  :config
  ;; capture templates
  (let ((templates
         '(("i" "info" entry (file gtd/inbox)    "* [#D] %?\n  %a\n%i\n")
           ("t" "todo" entry (file gtd/inbox)    "* TODO [#C] %?")
           ("w" "waiting" entry (file gtd/inbox) "* WAITING [#C] %?")
           ("p" "project" entry (file gtd/inbox) "* PROJECT [#B] %?")
           ("s" "schedule" entry (file gtd/inbox) "* TODO [#C] %?\n  SCHEDULED:%T\n")
           ("d" "deadline" entry (file gtd/inbox) "* TODO [#C] %?\n  DEADLINE:%T\n")
           ("r" "interrupt" entry (file+headline gtd/archive "Interrupt")
            "* DONE %?" :clock-in t :clock-resume t))))
    (dolist (tpl templates)
      (unless (assoc (car tpl) org-capture-templates)
        (add-to-list 'org-capture-templates tpl t))))

  ;; refile targets
  (setq org-refile-targets '((nil . (:maxlevel . 2))))

  ;; log type
  (setq org-log-done nil
        org-log-refile nil
        org-reverse-note-order nil
        ;; forbide capture/refile bookmark
        org-capture-bookmark nil
        org-bookmark-names-plist nil)

  ;; capture hook: Set Effort and Timestamp
  (add-hook 'org-capture-prepare-finalize-hook #'org-agenda/set-effort-and-time)
  ;; capture hook: Auto Refile
  (add-hook 'org-capture-after-finalize-hook #'org-agenda/auto-refile))

(provide 'init-gtd-workflow)
