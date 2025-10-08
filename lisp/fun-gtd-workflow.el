;;; fun-gtd-workflow-fun.el --- fun for gtd workflow -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun no-message (fun &rest args)
  "Apply FUN to ARGS, skipping message."
  (let ((inhibit-message t)      ;Don't show the messages in Echo area
        (message-log-max nil))   ;Don't show the messages in the *Messages* buffer
    (apply fun args)))
(defun no-confirm (fun &rest args)
  "Apply FUN to ARGS, skipping user confirmations."
  (cl-flet ((always-yes (&rest _) t))
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
              ((symbol-function 'yes-or-no-p) #'always-yes))
      (apply fun args))))

;;----------------------------------------
;;; Transient menus
;;----------------------------------------
(use-package org-agenda
  :after org
  :ensure nil
  :config
  (transient-define-prefix transient/org-agenda-mode()
    [["filter"
      ("a" "agenda act" transient/org-agenda-a :if (lambda() (org-agenda-check-type nil 'agenda)) :transient t)
      ("f" "filter" transient/org-filter :transient t)
      ("j" "quick filter" (lambda() (interactive)
                            (org-agenda-filter-remove-all)
                            (org-agenda-filter)))]

     ["statistics"
      ("l" "statistics" transient/org-agenda-statistics :transient t)]

     ["add info"
      ("t" "todo" org-agenda-todo)
      (":" "tag" org-agenda-set-tags)
      ("-" "-priority" org-agenda-priority-down)
      ("=" "+priority" org-agenda-priority-up)
      ("p" "property set" org-agenda-set-property)
      ("e" "effort" org-agenda-set-effort)
      ("d" "archive done" org-agenda/archive)]

     ["timestamp"
      ("s" "timestamp" transient/org-agenda-timestamp)]

     ["clock"
      ("SPC" "clock in" (lambda() (interactive)
			  (org-agenda-clock-in)
			  (org-agenda-redo t)))
      ("RET" "clock out" (lambda() (interactive)
			   (org-agenda-clock-out)
			   (org-agenda-redo t)))
      ("c" "clock cancel" org-agenda-clock-cancel)
      ("g" "clock go" org-agenda-clock-goto)]])

  (transient-define-prefix transient/org-agenda-a()
    [["view"
      ("d" "day" (lambda() (interactive) (org-agenda-goto-today) (org-agenda-day-view)))
      ("w" "week" org-agenda-week-view)
      ("W" "fortnight" org-agenda-fortnight-view)
      ("m" "month" org-agenda-month-view)
      ("y" "year" org-agenda-year-view)]
     ["nav"
      ("g" "go data" org-agenda-goto-date)
      ("b" "previous" org-agenda-earlier)
      ("f" "next" org-agenda-later)]])
  (transient-define-prefix transient/org-agenda-statistics()
    [["clock report"
      ("d" "day" (lambda() (interactive)
                   (kill-buffer org-agenda-buffer)
                   (let ((display-buffer-alist nil))
                     (org-agenda nil "a")
                     (org-agenda-clockreport-mode))))
      ("w" "week" (lambda() (interactive)
                    (kill-buffer org-agenda-buffer)
                    (let ((display-buffer-alist nil))
                      (org-agenda nil "a")
                      (org-agenda-week-view)
                      (org-agenda-clockreport-mode))))
      ("m" "month" (lambda() (interactive)
                     (kill-buffer org-agenda-buffer)
                     (let ((display-buffer-alist nil))
                       (org-agenda nil "a")
                       (org-agenda-month-view)
                       (org-agenda-clockreport-mode))))]
     ["log"
      ("l" "!log" (lambda() (interactive)
                    (if (org-agenda-check-type nil 'agenda)
                        (org-agenda-log-mode 'clockcheck)
                      (kill-buffer org-agenda-buffer)
                      (let ((display-buffer-alist nil))
                        (org-agenda nil "a")
                        (org-agenda-log-mode 'clockcheck)))))]
     ["project"
      ("p" "project" (lambda() (interactive)
                       (kill-buffer org-agenda-buffer)
                       (let ((display-buffer-alist nil))
                         (org-agenda nil "p")
                         (org-agenda-columns)
                         (org-agenda-next-item 1))))
      ("a" "archive" (lambda() (interactive)
                       (kill-buffer org-agenda-buffer)
                       (let ((display-buffer-alist nil))
                         (org-agenda nil "r")
                         (org-agenda-columns)
                         (org-agenda-next-item 1))))]])
  (transient-define-prefix transient/org-filter()
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
  (transient-define-prefix transient/org-agenda-timestamp()
    ["timestamp"
     ("s" "schedule" org-agenda-schedule)
     ("d" "deadline" org-agenda-deadline)])

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
;;; 快速切换agenda buffer
;;----------------------------------------
(defvar org-agenda/current-buffer-number 1)
(defun org-agenda/set-buffer-number(number)
  (setq org-agenda/current-buffer-number number))

(defun org-agenda/forward-buffer ()
  "agenda-view, next-view, inbox-view"
  (interactive)
  (unless (derived-mode-p 'org-agenda-mode)
    (user-error "Can only append from within agenda buffer"))
  (let ((org-agenda-multi nil))
    (pcase org-agenda/current-buffer-number
      (1 (org-agenda nil "n"))
      (2 (let ((inbox
                (with-current-buffer (find-buffer-visiting my/file-inbox)
                  (goto-char (point-min))
                  (re-search-forward "* " nil t))))
           (if inbox
	       (org-agenda nil "i")
             (org-agenda nil "a"))))
      (3 (org-agenda nil "a")))))

;;----------------------------------------
;;; Capture Hook: Set Effort/timestamp
;;----------------------------------------
(defun org-agenda/set-effort-and-time ()
  (let ((todo-key (org-get-todo-state))
        (time-string (format-time-string
                      "<%Y-%m-%d %H:%M>"
                      (time-add (current-time) (days-to-time 0)))))
    (goto-char (point-min))
    (org-set-property "CAPTURE_TIME" time-string)
    (when (member todo-key '("TODO" "WAITING" "PROJECT"))
      (org-set-effort))))

;;----------------------------------------
;;; Capture Hook: Auto Refile
;;----------------------------------------
(defun org-agenda/auto-refile ()
  "Automatically refile TODO headings from inbox.org.

Rules:
1. If heading has no tags → refile to `my/file-task`.
2. If heading has tags → find PROJECT entry in GTD files
   where ITEM matches any tag, then refile under that project."
  (with-current-buffer (find-buffer-visiting my/file-inbox)
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (let ((tags (org-get-tags))
              (todo (org-get-todo-state))
              (pos nil))
          (when todo
            (if (not tags)
                (org-refile nil nil (list nil my/file-task nil nil))
              (catch 'done
                (dolist (tag tags)
                  (dolist (file gtd/no-common-files)
                    (message "-----------------%s" file)
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

;;----------------------------------------
;;; Org Agenda Hook: Delete Empty Blocks
;;----------------------------------------
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

;;----------------------------------------
;;; Org Agenda 'a' Hook: goto 'n'
;;----------------------------------------
(defun org-agenda/a-hook ()
  (when (= org-agenda/current-buffer-number 1)
    (with-current-buffer org-agenda-buffer
      (unless (next-single-property-change (line-end-position) 'org-marker)
        (org-agenda nil "n")))))

;;----------------------------------------
;;; Utility Functions
;;----------------------------------------
;; 输入农历日期, 返回对应的阳历日期
(defun my/lunar (lunar-month lunar-day)
  (let* ((current-month (car (calendar-current-date)))
	 (current-year (cadr (cdr (calendar-current-date))))
	 (cn-years (calendar-chinese-year ; calendar-chinese-year counts from 12 for last year
		    (if (and (eq current-month 12) (eq lunar-month 12))
			(1+ current-year)
		      current-year)))
	 (run-yue (assoc lunar-month cn-years))
	 (date (calendar-gregorian-from-absolute
		(+ (cadr run-yue) (1- lunar-day)))))
    date))

;;----------------------------------------
;;; Functions For 'org-agenda-custom-commands'
;;----------------------------------------
;;;; skip entry
(defun org-agenda/skip-entry()
  "Skip entry when its root-parent todo-state is CANCEL or DONE."
  (org-back-to-heading t)
  (let* ((end (org-entry-end-position)))
    (if (org-entry/root-head-done)
        end
      nil)))
(defun org-entry/root-head-done ()
  (while (> (org-outline-level) 1)
    (outline-up-heading 1 t))
  (org-entry-is-done-p))
(defun my/org-agenda-skip-future-entry()
  "Skip entry when its `'org-time-stamp' is futher than today."
  (org-back-to-heading t)
  (let* ((end (org-entry-end-position))
         (dif (org-time-stamp-to-now (org-entry-get nil "ITEM"))))
    (if (= dif 0)
        nil
      end)))

;;;; 返回agenda (n) 中 item的前缀信息
(defun my/org-agenda-pf-next ()
  "Used by `org-agenda-custom-commands' (init-org-agenda-mode.el)
Function: return time_duration since capture_time"
  (let* ((capture_time (org-entry-get nil "CAPTURE_TIME"))
	 (v1 "")
	 (start-level (funcall outline-level))
	 (v2 ""))
    ;; capture_time format
    (when capture_time
      (let* ((dura_time (org-time-since capture_time))
	     (h_seconds (car dura_time))
	     (seconds (cadr dura_time))
	     (seconds (+ seconds (* h_seconds 65535) ))
	     (day (/ seconds 86400))
	     (seconds (- seconds (* day 86400)))
	     (hour (/ seconds 3600))
	     (seconds (- seconds (* hour 3600)))
	     (minute (/ seconds 60)) )

	(when (> minute 0)
	  (setq v1 (concat (number-to-string minute) "m")))
	(when (> hour 0)
	  (setq v1 (concat (number-to-string hour) "h")))
	(when (> day 0)
	  (setq v1 (concat (number-to-string day) "d")))))
    ;; top-parent P_UUID format
    (when (> start-level 1)
      (outline-up-heading '16 t)
      (setq v2 (org-entry-get nil "ITEM")))

    (format "%-6s%-9s" v1 v2 )))
(defun my/org-agenda-pf-next-p ()
  "Used by `org-agenda-custom-commands' (init-org-agenda-mode.el)
Function: return %-10c."
  (let* ((category (org-get-category)))
    (format "%-15s" category)))
(defun my/org-agenda-pf-project ()
  "Used by `org-agenda-custom-commands' (init-org-agenda-mode.el)
Function: return time_duration since capture_time"
  (let* ((capture_time (org-entry-get nil "CAPTURE_TIME"))
	 (v1 "")
         (todo-state (org-get-todo-state))
	 (v2 ""))
    ;; capture_time format
    (when capture_time
      (let* ((dura_time (org-time-since capture_time))
	     (h_seconds (car dura_time))
	     (seconds (cadr dura_time))
	     (seconds (+ seconds (* h_seconds 65535) ))
	     (day (/ seconds 86400))
	     (seconds (- seconds (* day 86400)))
	     (hour (/ seconds 3600))
	     (seconds (- seconds (* hour 3600)))
	     (minute (/ seconds 60)) )

	(when (> minute 0)
	  (setq v1 (concat (number-to-string minute) "m")))
	(when (> hour 0)
	  (setq v1 (concat (number-to-string hour) "h")))
	(when (> day 0)
	  (setq v1 (concat (number-to-string day) "d")))))

    (when (string= todo-state "WAITING")
      (setq v2 "wait"))
    (format "%-6s%-9s" v1 v2)))


(provide 'fun-gtd-workflow)
;;; fun-gtd-workflow-fun.el ends here
