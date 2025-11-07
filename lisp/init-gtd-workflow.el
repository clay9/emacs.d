;;; init-gtd-workflow-fun.el --- GTD workflow by Org Agenda -*- lexical-binding: t -*-
;;; Commentary:
;; Org-agenda configuration for GTD workflow
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

  ;; Show/hide clock display
  (require 'sub-gtd-clock-display))

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
  (defun org-agenda/set-effort-and-time ()
    (let ((todo-key (org-get-todo-state))
          (time-string (format-time-string
                        "[%Y-%m-%d %H:%M]"
                        (time-add (current-time) (days-to-time 0)))))
      (goto-char (point-min))
      (org-set-property "CAPTURE_TIME" time-string)
      (when (member todo-key '("TODO" "WAITING" "PROJECT"))
        (org-set-effort))))
  (add-hook 'org-capture-prepare-finalize-hook #'org-agenda/set-effort-and-time)

  ;; capture hook: Auto Refile
  (defun org-agenda/auto-refile ()
    "Automatically refile TODO headings from inbox.org.

Rules:
1. If heading has no tags → refile to `gtd/task`.
2. If heading has tags → find PROJECT entry in GTD files
   where ITEM matches any tag, then refile under that project."
    (with-current-buffer (find-buffer-visiting gtd/inbox)
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (let ((tags (org-get-tags))
                (todo (org-get-todo-state))
                (pos nil))
            (when todo
              (if (not tags)
                  (org-refile nil nil (list nil gtd/task nil nil))
                (catch 'done
                  (dolist (tag tags)
                    (dolist (file gtd/projects)
                      ;; find pos in file
                      (with-current-buffer (find-file-noselect file)
                        (org-with-wide-buffer
                         (catch 'found
                           (org-map-entries
                            (lambda ()
                              (when (and (string= "PROJECT" (org-get-todo-state))
                                         (string= tag (org-entry-get nil "ITEM")))
                                (setq pos (point))
                                (throw 'found t)))
                            nil 'file))))
                      ;; when found pos, return
                      (when pos
                        (org-toggle-tag tag 'off)
                        (org-refile nil nil (list nil file nil pos))
                        (throw 'done t)))))))))
        nil 'file))))
  (add-hook 'org-capture-after-finalize-hook #'org-agenda/auto-refile))

(provide 'init-gtd-workflow)
