;;; init-org-agenda-mode.el --- Org-agenda -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; files

(defvar my/gtd-dir "~/my/gtd/")
(defvar my/file-inbox   (expand-file-name "gtd_common/inbox.org"   my/gtd-dir))
(defvar my/file-task    (expand-file-name "gtd_common/task.org"    my/gtd-dir))
(defvar my/file-archive (expand-file-name "gtd_common/archive.org" my/gtd-dir))
(defvar my/file-diary   (expand-file-name "diary.org"              my/gtd-dir))
(defvar my/file-life    (expand-file-name "life.org"               my/gtd-dir))

(require 'init-org-agenda-fun)


;;; todo && tag && priority

(with-eval-after-load 'org
  ;; todo keywords
  (setq org-todo-keywords
        '((type  "TODO(t)" "WAITING(w)" "PROJECT(p)" "|"  "DONE(d)" "CANCEL(c)")))
  (setq org-enforce-todo-dependencies t)
  (setq org-closed-keep-when-no-todo 'nil)
  (setq org-use-fast-todo-selection 'expert)

  ;; priority
  (setq org-highest-priority ?A
        org-lowest-priority  ?D
        org-default-priority ?D)
  (setq org-priority-faces
        '((?A . (:background "" :foreground "red" :weight bold))
	  (?B . (:background "" :foreground "DarkOrange" :weight bold))
	  (?C . (:background "" :foreground "yellow" :weight bold))
	  (?D . (:background "" :foreground "DodgerBlue" :weight bold)))))


;;; clock && effort

(with-eval-after-load 'org
  (setq org-global-properties
      (quote (("Effort_ALL" . "0:00 0:15 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00"))))

  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Save state changes in the LOGBOOK drawer
  (setq org-log-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; when clock in, set effort
  (defun my/set-effort-when-clock-in ()
    (let ((v (org-entry-properties nil "EFFORT"))
	  (todo_key (org-get-todo-state)))
      (when (and (not (string= todo_key "DONE"))
	         (or (not v)
		     (string= (cdr (car v)) "0:00")))
        (org-set-effort))))
  (add-hook 'org-clock-in-hook 'my/set-effort-when-clock-in)

  ;; Show|Hide the clocked-in task - if any - in the header line
  (defun my/show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))
  (defun my/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))
  (add-hook 'org-clock-in-hook 'my/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook 'my/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook 'my/hide-org-clock-from-header-line))

(with-eval-after-load 'org-agenda
  ;; skip zero-time in clock-report
  (setq org-agenda-clockreport-parameter-plist '(:stepskip0 t :link t :maxlevel 3 :fileskip0 t)))


;;; capture, refile, archive

(with-eval-after-load 'org-capture
  ;; capture
  (setq org-capture-templates
        '(("i" "info" entry (file my/file-inbox)     "* [#D] %?\n  %a\n%i\n")
	  ("t" "todo" entry (file my/file-inbox)     "* TODO [#C] %?")
	  ("w" "waiting" entry (file my/file-inbox)  "* WAITING [#C] %?")
	  ("p" "project" entry (file my/file-inbox)  "* PROJECT [#B] %?")
	  ("s" "schedule" entry (file my/file-inbox) "* TODO [#C] %?\n  SCHEDULED:%T\n")
	  ("d" "deadline" entry (file my/file-inbox) "* TODO [#C] %?\n  DEADLINE:%T\n")
	  ("j" "diary" entry (file+datetree my/file-diary) "* %?\n  %T\n  ")
	  ("r" "interrupt" entry (file+headline my/file-archive "Interrupt")
	   "* DONE %?" :clock-in t :clock-resume t)))

  ;; refile => allow refile in current buffer
  (setq org-refile-targets '((nil . (:maxlevel . 2))))

  ;; archive

  ;; log type
  (setq org-log-done 'nil)
  (setq org-log-refile 'nil)
  ;; store new notes at the begin
  (setq org-reverse-note-order nil)

  ;; forbide capture/refile bookmark
  (setq org-capture-bookmark nil)
  (setq org-bookmark-names-plist nil)

;;; capture hook
  (defun my/org-capture-prepare-finalize-hook()
    (let ((todo_key (org-get-todo-state)))
      (goto-char (point-min))
      (org-set-property "CAPTURE_TIME" (my/org-timestamp-string 0 t))
      (when (or (string= todo_key "TODO")
	        (string= todo_key "WAITING")
                (string= todo_key "PROJECT"))
        (org-set-effort))))
  (add-hook 'org-capture-prepare-finalize-hook 'my/org-capture-prepare-finalize-hook))


;;; agenda files

(with-eval-after-load 'org-agenda
  (setq org-agenda-files (let ((rlist))
                               (when (file-directory-p my/gtd-dir)
                                 (dolist (file (directory-files my/gtd-dir nil "^gtd"))
                                   (push (concat my/gtd-dir file) rlist))
                                 rlist)))

  ;; ignore tags in agenda buffer
  (setq org-agenda-hide-tags-regexp
        "emacs\\|org\\|ccIDE\\|qygame\\|habit"))


;;; agenda-view

(with-eval-after-load 'org-agenda
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
  (setq org-agenda-deadline-leaders '("Dead" "In %dd" "Dead %dd")))

;;; agenda buffers

(with-eval-after-load 'org-agenda
  ;; window setup
  (setq org-agenda-window-setup 'current-window)
  ;; not show sublevels in agenda
  (setq org-habit-show-habits-only-for-today t)
  ;; diary
  (setq org-agenda-include-diary nil
        org-agenda-diary-file my/file-diary)

  ;; agenda buffers
  (setq org-agenda-custom-commands
        '(("a" "agenda"
	   ((agenda ""))
	   ((_ (my/org-agenda-set-buffer-number 1))
            (org-agenda-files (delete (expand-file-name "gtd_common/archive.org" my/gtd-dir) (org-agenda-files)))
	    (org-agenda-prefix-format "%-10c%?-12t%?-12s")
	    (org-agenda-skip-scheduled-if-done t)
	    (org-agenda-skip-deadline-if-done  t)
	    (org-deadline-warning-days 2)
	    (org-agenda-sorting-strategy '(time-up todo-state-up scheduled-up deadline-up priority-up))))
	  ("n" "Next Step"
	   ((todo "TODO"
		  ((org-agenda-overriding-header "TODO")
                   (org-agenda-files (delete (expand-file-name "gtd/emacs.org" my/gtd-dir)
                                             (delete (expand-file-name "gtd/qygame.org" my/gtd-dir)
                                                     (delete (expand-file-name "gtd/yygame.org" my/gtd-dir) (org-agenda-files)))))
                   (org-agenda-skip-function `(my/org-agenda-skip-entry))                   
                   (org-agenda-prefix-format "%(my/org-agenda-pf-next)")))
	    (todo "WAITING"
		  ((org-agenda-overriding-header "WAITING")
                   (org-agenda-files (delete (expand-file-name "gtd/emacs.org" my/gtd-dir)
                                             (delete (expand-file-name "gtd/qygame.org" my/gtd-dir)
                                                     (delete (expand-file-name "gtd/yygame.org" my/gtd-dir) (org-agenda-files)))))
                   (org-agenda-prefix-format "%(my/org-agenda-pf-next)")))
            (todo "TODO|WAITING"
		  ((org-agenda-overriding-header "PROJECT emacs")
                   (org-agenda-files (list (expand-file-name "gtd/emacs.org" my/gtd-dir)))
		   (org-agenda-skip-function `(my/org-agenda-skip-entry))
                   (org-agenda-prefix-format "%(my/org-agenda-pf-project)")))
            (todo "TODO|WAITING"
		  ((org-agenda-overriding-header "PROJECT qygame")
                   (org-agenda-files (list (expand-file-name "gtd/qygame.org" my/gtd-dir)))
		   (org-agenda-skip-function `(my/org-agenda-skip-entry))
                   (org-agenda-prefix-format "%(my/org-agenda-pf-project)")))
            (todo "TODO|WAITING"
		  ((org-agenda-overriding-header "PROJECT yygame")
                   (org-agenda-files (list (expand-file-name "gtd/yygame.org" my/gtd-dir)))
		   (org-agenda-skip-function `(my/org-agenda-skip-entry))
                   (org-agenda-prefix-format "%(my/org-agenda-pf-project)")))
	    (todo "PROJECT"
	          ((org-agenda-overriding-header "STUCK PROJECT")
                   (org-agenda-files (delete (expand-file-name "gtd/emacs.org" my/gtd-dir)
                                             (delete (expand-file-name "gtd/qygame.org" my/gtd-dir)
                                                     (delete (expand-file-name "gtd/yygame.org" my/gtd-dir) (org-agenda-files)))))         
	           (org-agenda-prefix-format "%(my/org-agenda-pf-next-p)")
	           (org-agenda-skip-function `(org-agenda-skip-subtree-if 'todo '("TODO" "WAITING"))))))
	   ((_ (my/org-agenda-set-buffer-number 2))
            (org-agenda-todo-keyword-format "")
	    (org-agenda-tags-todo-honor-ignore-options t)
	    (org-agenda-todo-ignore-scheduled 'all)
	    (org-agenda-todo-ignore-deadlines 'all)
	    (org-agenda-todo-ignore-timestamp 'all)
	    (org-agenda-sorting-strategy '(priority-down effort-up))))
	  ("i" "inbox"
	   ((search "* "))
	   ((_ (my/org-agenda-set-buffer-number 3))
	    (org-agenda-overriding-header "Inbox")
	    (org-agenda-prefix-format "%(my/org-agenda-pf-next)")
	    (org-agenda-files (list my/file-inbox))
	    (org-agenda-sorting-strategy '(priority-down alpha-up effort-up))))
	  ("p" "project"
	   ((tags "LEVEL=1/PROJECT"
		  ((org-agenda-overriding-header "PROJECT")))
	    (tags "LEVEL=1/DONE"
		  ((org-agenda-overriding-header "PROJECT - DONE")))
	    (tags "LEVEL=1/CANCEL"
		  ((org-agenda-overriding-header "PROJECT - CANCEL"))))
	   ((_ (my/org-agenda-set-buffer-number 4))
	    (org-agenda-prefix-format "%-10c")
            (org-agenda-files (delete my/file-archive (org-agenda-files)))
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
	   ((_ (my/org-agenda-set-buffer-number 5))
	    (org-agenda-sorting-strategy '(category-keep))
	    (org-agenda-files (list my/file-archive))
	    (org-agenda-prefix-format "")
	    (org-agenda-todo-keyword-format "")
	    (org-overriding-columns-format "%24ITEM %1PRIORITY %10TAGS %Effort %10CLOCKSUM %22ARCHIVE_TIME"))))))


;;; hook -- TODO: not work

(with-eval-after-load 'org
  ;; when kill emacs, save org-files
  (add-hook 'kill-emacs-hook 'org-save-all-org-buffers))


;;; org columns

(with-eval-after-load 'org-colview
  (setq org-columns-default-format "%24ITEM %7TODO %1PRIORITY %10TAGS %Effort %10CLOCKSUM")
  (set-face-attribute 'org-column nil
                      :background (face-attribute 'default :background)
		      :height (face-attribute 'default :height)
		      :family (face-attribute 'default :family)))


;;; shortkey

(with-eval-after-load 'org-agenda
  (defun transient/org-agenda/archive ()
    (interactive)
    ;; 1. close clock if in this item TODONOW
    (when (and (org-clock-is-active)
               (string= (my/org-agenda-get-property nil "ITEM") org-clock-current-task))
      (org-agenda-clock-out))
    ;; 2. move to %project%_archive
    (org-agenda-archive)
    ;; 3. revert org-agenda buff
    (my/org-agenda-redo))
  (transient-define-prefix transient/org-agenda-a()
    [["view"
      ("d" "day" (lambda() (interactive) (org-agenda-goto-today) (org-agenda-day-view)))
      ("w" "week" org-agenda-week-view)
      ("W" "fortnight" org-agenda-fortnight-view)
      ("m" "month" org-agenda-month-view)
      ("y" "year" org-agenda-year-view)]
     ["nav"
      ("g" "go data" org-agenda-goto-date)
      ("-" "previous" my/org-agenda-dwmy-view-previous)
      ("=" "next" my/org-agenda-dwmy-view-next)]])
  (transient-define-prefix transient/org-agenda-statistics()
    [["clock report"
      ("d" "day" (lambda() (interactive)
                   (org-agenda nil "a")
                   (org-agenda-clockreport-mode)))
      ("w" "week" (lambda() (interactive)
                    (org-agenda nil "a")
                    (org-agenda-week-view)
                    (org-agenda-clockreport-mode)))
      ("m" "month" (lambda() (interactive)
                     (org-agenda nil "a")
                     (org-agenda-month-view)
                     (org-agenda-clockreport-mode)))]
     ["log"
      ("l" "!log" (lambda() (interactive) (org-agenda-log-mode 'clockcheck)))]
     ["project"
      ("p" "project" (lambda() (interactive)
                       (org-agenda nil "p")
                       (org-agenda-columns)
                       (org-agenda-next-item 1)))
      ("a" "archive" (lambda() (interactive)
                       (org-agenda nil "r")
                       (org-agenda-columns)
                       (org-agenda-next-item 1)))]])
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
      ("d" "archive done" transient/org-agenda/archive)]

     ["timestamp"
      ("s" "timestamp" transient/org-agenda-timestamp)]

     ["clock"
      ("SPC" "clock in" (lambda() (interactive)
			  (org-agenda-clock-in)
			  (my/org-agenda-redo)))
      ("RET" "clock out" (lambda() (interactive)
			   (org-agenda-clock-out)
			   (my/org-agenda-redo)))
      ("c" "clock cancel" org-agenda-clock-cancel)
      ("g" "clock go" org-agenda-clock-goto)]])

  (define-key org-agenda-mode-map (kbd "C-j") 'transient/org-agenda-mode)

  ;; previous && next
  (define-key org-agenda-mode-map (kbd "p") 'org-agenda-previous-item)
  (define-key org-agenda-mode-map (kbd "n") 'org-agenda-next-item)

  ;; choose agenda view
  (define-key org-agenda-mode-map (kbd "SPC") 'my/org-agenda-forward)

  ;; entry show && entry enter
  (define-key org-agenda-mode-map (kbd "TAB") 'my/org-agenda-show)
  (define-key org-agenda-mode-map (kbd "RET") 'my/org-agenda-enter)
  (define-key org-agenda-mode-map (kbd "a") 'my/org-agenda-entry-text-show)

  ;; quit && refresh
  (define-key org-agenda-mode-map (kbd "q") #'(lambda() (interactive)
                                                (let ((win (selected-window)))
                                                  (org-agenda-quit)
                                                  (delete-window win))))
  (define-key org-agenda-mode-map (kbd "r") 'my/org-agenda-redo))

(with-eval-after-load 'org-colview
  ;; previous && next
  (define-key org-columns-map (kbd "p") 'org-agenda-previous-item)
  (define-key org-columns-map (kbd "n") 'org-agenda-next-item))


(provide 'init-org-agenda-mode)
;;; init-org-agenda-mode.el ends here
