;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'org-agenda)
(require 'init-org-agenda-fun);; 加载重载函数

;; ****************************************************
;; files, todo, tag, priority
;; ****************************************************
;; agenda files
(setq my-file-inbox   (expand-file-name "gtd_common/inbox.org"   my/gtd-dir))
(setq my-file-task    (expand-file-name "gtd_common/task.org"    my/gtd-dir))
(setq my-file-archive (expand-file-name "gtd_common/archive.org" my/gtd-dir))
;; gtd_common:通用(inbox, task, archive);  gtd_my: self projects;  gtd_work: public projects
(setq org-agenda-files (list (expand-file-name "gtd_common/" my/gtd-dir)
			     (expand-file-name "gtd_my/"     my/gtd-dir)
			     (expand-file-name "gtd_work/"   my/gtd-dir)))
(setq my-file-diary   (expand-file-name "diary/diary.org"    my/gtd-dir))

;; todo keywords
(setq org-todo-keywords
      '((type  "TODO(t)" "WAITING(w)" "PROJECT(p)" "|"  "DONE(d)" "CANCEL(c)")))
;; only sub done, parents done
(setq org-enforce-todo-dependencies t)
(setq org-closed-keep-when-no-todo 'nil)

;; ignore tags in agenda buffer
(setq org-agenda-hide-tags-regexp
      "emacs\\|org\\|ccIDE\\|qygame\\|habit")


;; 设置Priority[min, max] && default priority
(setq org-highest-priority ?A)
(setq org-lowest-priority  ?D)
(setq org-default-priority ?D)
(setq org-priority-faces
      '((?A . (:background "" :foreground "red" :weight bold))
	(?B . (:background "" :foreground "DarkOrange" :weight bold))
	(?C . (:background "" :foreground "yellow" :weight bold))
	(?D . (:background "" :foreground "DodgerBlue" :weight bold))
	))
;; 设置todo中 active-project样式
(set-face-attribute 'org-agenda-dimmed-todo-face nil :foreground "green3")

;; ****************************************************
;; capture, refile, archive, log
;; ****************************************************
;; capture
(setq org-capture-templates
      '(("i" "info" entry (file my-file-inbox)     "* [#D] %?\n  %a\n%i\n")
	("t" "todo" entry (file my-file-inbox)     "* TODO [#C] %?")
	("w" "waiting" entry (file my-file-inbox)  "* WAITING [#C] %?")
	("p" "project" entry (file my-file-inbox)  "* PROJECT [#B] %?")
	("s" "schedule" entry (file my-file-inbox) "* TODO [#C] %?\n  SCHEDULED:%T\n")
	("d" "deadline" entry (file my-file-inbox) "* TODO [#C] %?\n  DEADLINE:%T\n")
	("j" "diary" entry (file+datetree my-file-diary) "* %?\n  %T\n  ")
	("r" "interrupt" entry (file+headline my-file-archive "Interrupt")
	 "* DONE %?" :clock-in t :clock-resume t)))

;; refile
;; use org-refile RFLOC, org-refile-targets useless

;; archive
;; gtd_common: set by file;   gtd_my,gtd_work: not archive.
;;(setq org-archive-location (concat my-file-archive "::"))

;; log type
(setq org-log-done 'nil)
(setq org-log-refile 'nil)
; store new notes at the begin
(setq org-reverse-note-order nil)

; forbide capture/refile bookmark
(setq org-capture-bookmark nil)
(setq org-bookmark-names-plist nil)

;; ****************************************************
;; schedule, deadline
;; ****************************************************
;; 显示标准: day
(setq org-agenda-span 'day)

;; 设置agenda中 time-grid. 希望把time划分为上午, 下午, 晚上;
;; 12:00-14:00, 18:00-19:00 23:00之后不应该工作
(setq org-agenda-time-grid '((daily today require-timed) (800 1200 1400 1800 1900 2300) "......" "----------------"))
;; 设置agenda-view中 today的样式
(set-face-attribute 'org-agenda-date-today nil
		    :weight 'bold
		    :italic 'nil
		    :underline '(:color foreground-color :style line)
		    :inherit '(org-agenda-date))

;; 设置Schedule 和 Deadline的提示样式
(setq org-agenda-scheduled-leaders '("Start" "Start %dd"))
(setq org-agenda-deadline-leaders '("Dead" "In %dd" "Dead %dd"))

;; ****************************************************
;; org columns
;; ****************************************************
(setq org-columns-default-format "%24ITEM %7TODO %1PRIORITY %10TAGS %Effort %10CLOCKSUM")
(set-face-attribute 'org-column nil
			:height (face-attribute 'default :height)
			:family (face-attribute 'default :family))

;; ****************************************************
;; clock && effort
;; ****************************************************
(setq org-global-properties
      (quote (("Effort_ALL" . "0:00 0:15 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00"))))

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)


(defun my/set-effort-when-clock-in ()
  (let ((v (org-entry-properties nil "EFFORT"))
	(todo_key (org-get-todo-state)))
    (when (and (not (string= todo_key "DONE"))
	       (or (not v)
		   (string= (cdr (car v)) "0:00")))
      (org-set-effort))))
(add-hook 'org-clock-in-hook 'my/set-effort-when-clock-in)


;; ****************************************************
;; clock report
;; ****************************************************
;; skip zero-time in clock-report
(setq org-agenda-clockreport-parameter-plist '(:stepskip0 t :link t :maxlevel 3 :fileskip0 t))


;; ****************************************************
;; agenda buffers
;; ****************************************************
;; not show sublevels in agenda
(setq org-habit-show-habits-only-for-today t)

(setq org-agenda-custom-commands
      '(("a" "agenda"
	 ((agenda ""))
	 ((local_temp (funcall set_org_buffer_no 1))
	  (org-agenda-prefix-format "%-10c%?-12t%?-12s")
	  (org-agenda-skip-scheduled-if-done t)
	  (org-agenda-skip-deadline-if-done  t)
	  (org-deadline-warning-days 2)
	  (org-agenda-files (cons my-file-diary (org-agenda-files)))
	  (org-agenda-sorting-strategy '(time-up todo-state-up scheduled-up deadline-up priority-up))))
	("n" "Next Step"
	 ((todo "TODO"
		((org-agenda-overriding-header "TODO")))
	  (todo "WAITING"
		((org-agenda-overriding-header "WAITING")))
	  (todo "PROJECT"
		((org-agenda-overriding-header "PROJECT")
		 (org-agenda-prefix-format "%(my/org-agenda-pf-next-p)%l%l")
		 (org-agenda-skip-function
		  #'(lambda ()
		      (when (string= (org-entry-get nil "HIDE_WHEN_STUCK") "yes")
			(org-agenda-skip-subtree-if 'nottodo '("TODO" "WAITING"))))))))
	 ((local_temp (funcall set_org_buffer_no 2))
	  (org-agenda-todo-keyword-format "")
	  (org-agenda-prefix-format "%(my/org-agenda-pf-next)")
	  (org-agenda-files (delete my-file-inbox (org-agenda-files)))
	  (org-agenda-tags-todo-honor-ignore-options t)
	  (org-agenda-todo-ignore-scheduled 'all)
	  (org-agenda-todo-ignore-deadlines 'all)
	  (org-agenda-todo-ignore-timestamp 'all)
	  (org-agenda-sorting-strategy '(priority-down effort-up))))
	("i" "inbox"
	 ((search "* "))
	 ((local_temp (funcall set_org_buffer_no 3))
	  (org-agenda-overriding-header "Inbox")
	  (org-agenda-prefix-format "%(my/org-agenda-pf-next)")
	  (org-agenda-files (list my-file-inbox))
	  (org-agenda-sorting-strategy '(priority-down alpha-up effort-up))))
	("p" "project"
	 ((tags "CAPTURE_TODO={PROJECT}|TODO={PROJECT}/-DONE-CANCEL"
		     ((org-agenda-overriding-header "PROJECT")))
	  (tags "CAPTURE_TODO={PROJECT}|TODO={PROJECT}/DO"
		     ((org-agenda-overriding-header "PROJECT - DONE")))
	  (tags "CAPTURE_TODO={PROJECT}|TODO={PROJECT}/CANCEL"
		((org-agenda-overriding-header "PROJECT - CANCEL"))))
	 ((local_temp (funcall set_org_buffer_no 4))
	  (org-agenda-prefix-format "%-10c")
	  (org-agenda-todo-keyword-format "")
	  (org-overriding-columns-format "%24ITEM %10CATEGORY %1PRIORITY %10TAGS %Effort %10CLOCKSUM %22ARCHIVE_TIME")
	  (org-agenda-sorting-strategy '(category-keep))))
	("r" "archive"
	 ((todo "DONE"
		((org-agenda-overriding-header "DONE")))
	  (todo "CANCEL"
		((org-agenda-overriding-header "CANCEL")))
	  (tags "-TODO={DONE\\|CANCEL}+LEVEL=1"
		((org-agenda-overriding-header "Others")
		 (org-tags-match-list-sublevels nil) )))
	 ((local_temp (funcall set_org_buffer_no 5))
	  (org-agenda-sorting-strategy '(category-keep))
	  (org-agenda-files (list my-file-archive))
	  (org-agenda-prefix-format "%-10(my/org-agenda-pf-archive)")
	  (org-agenda-todo-keyword-format "")
	  (org-overriding-columns-format "%24ITEM %1PRIORITY %10TAGS %Effort %10CLOCKSUM %22ARCHIVE_TIME")
	  ))))

;; ****************************************************
;; others
;; ****************************************************
;; diary
(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file my-file-diary)

;; agenda的出现模式
(setq org-agenda-window-setup 'current-window)

;; ****************************************************
;; hook
;; ****************************************************
;; when kill emacs, sort && save  org-files
(defun my/save-org-files-when-kill-emacs()
  ;; don't sort
  ;;  (dolist (file (org-agenda-files))
  ;;    (my-org-sort-entries file))
  (org-save-all-org-buffers))
(add-hook 'kill-emacs-hook 'my/save-org-files-when-kill-emacs)


;; hook after org-agenda  show
(defun my/org-agenda-finalize-hook()
  (move-end-of-line 1)
  ;; note when no clock active
  (when (not (org-clock-is-active))
    (let ((note "     NOT IN CLOCK"))
      (put-text-property 0 (length note) 'face 'error note)
      (insert note)))
  ;; choose next item
  (org-agenda-next-item 1))
(add-hook 'org-agenda-finalize-hook 'my/org-agenda-finalize-hook)




;; hook for capture
(defun my/org-time-stamp-string ()
  "Get current time stamp"
   (with-temp-buffer
     (org-mode)
     (org-insert-time-stamp (current-time) t t)
     (buffer-substring (point-min) (point-max))))
(defun my/org-capture-prepare-finalize-hook()
  (let ((todo_key (org-get-todo-state)))
    (goto-char (point-min))
    (when todo_key (org-set-property "CAPTURE_TODO" todo_key))
    (org-set-property "CAPTURE_TIME" (my/org-time-stamp-string))
    (when (string= todo_key "PROJECT")
      (my/set-puuid-and-hws-to-project))
    (when (or (string= todo_key "TODO")
	      (string= todo_key "WAITING"))
      (org-set-effort))))
(add-hook 'org-capture-prepare-finalize-hook 'my/org-capture-prepare-finalize-hook)


;; hook for todo
(defun my/set-puuid-and-hws-to-project()
  "add puuid && hsw to PROJECT"
  (let ((puuid (org-entry-get nil "P_UUID"))
	(hws   (org-entry-get nil "HIDE_WHEN_STUCK")))
    (when (not puuid)
      (setq puuid (read-from-minibuffer "set P_UUID: "))
      (when puuid
	(org-set-property "P_UUID" puuid)))
    (when (not hws)
      (setq hws (read-from-minibuffer "hide when stuct (yes?): "))
      (when hws
	(org-set-property "HIDE_WHEN_STUCK" hws)))))
(defun my/org-add-puuid-to-project()
  "ask user if add P_UUID to PROJECT ENTRY"
  (when (string= org-state "PROJECT")
    (my/set-puuid-and-hws-to-project)))
(add-hook 'org-after-todo-state-change-hook 'my/org-add-puuid-to-project)


(provide 'init-org-agenda-mode)
