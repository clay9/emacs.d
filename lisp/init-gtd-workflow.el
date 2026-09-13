;;; init-gtd-workflow-fun.el --- GTD workflow by Org Agenda -*- lexical-binding: t -*-
;;; Commentary:
;; Org-agenda configuration for GTD workflow
;;; Code:

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
         '(("i" "info" entry (file gtd/inbox)     "* [#D] %?\n  %a\n%i\n")
           ("t" "todo" entry (file gtd/task)      "* TODO [#C] %?")
           ("w" "waiting" entry (file gtd/task)   "* WAITING [#C] %?")
           ("p" "project" entry (file gtd/task)   "* PROJECT [#B] %?")
           ("s" "schedule" entry (file gtd/task)  "* TODO [#C] %?\n  SCHEDULED:%T\n")
           ("d" "deadline" entry (file gtd/task)  "* TODO [#C] %?\n  DEADLINE:%T\n")
           ("r" "interrupt" entry (file+headline gtd/archive "Interrupt")
            "* DONE %?" :clock-in t :clock-resume t))))
    (dolist (tpl templates)
      (unless (assoc (car tpl) org-capture-templates)
        (add-to-list 'org-capture-templates tpl t))))

  ;; refile targets
  ;; (setq org-refile-targets '((nil . (:maxlevel . 2))))

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
  (add-hook 'org-capture-prepare-finalize-hook #'org-agenda/set-effort-and-time))


;;----------------------------------------
;;; Statistics
;;----------------------------------------
(defvar gtd/org-daily-clockreport
  "\n** Daily Report\n#+BEGIN: clocktable :scope agenda :block today :stepskip0 t :fileskip0 t :maxlevel 3\n#+END:\n")

(defun gtd/org-update-daily-clockreport-file ()
  (interactive)
  (let* ((filename (concat (format-time-string "%Y-%m-%d") ".jd"))
         (report-file (expand-file-name filename "~/my/gtd/journal")))
    (when (file-exists-p report-file)
      (let ((buf (find-file-noselect report-file)))
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (unless (search-forward "#+BEGIN: clocktable" nil t)
              (goto-char (point-max))
              (insert gtd/org-daily-clockreport)))
          (org-update-all-dblocks)
          ;; indent
          (goto-char (point-max))
          (org-indent-block)
          (save-buffer))
        (message "Daily clock report updated in %s at %s"
                 report-file (format-time-string "%H:%M:%S"))))))

;; 每日 23:30 自动静默运行
(run-at-time "23:30" 86400 #'gtd/org-update-daily-clockreport-file)

(provide 'init-gtd-workflow)
