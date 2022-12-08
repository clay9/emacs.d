;; ****************************************************
;; 返回agenda (n) 中 item的编号
;; ****************************************************
;; defun org-agenda-prefix-format
(defun my/org-agenda-pf-next ()
  "Used by org-agenda-custom-commands (init-org-agenda-mode.el)
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
      (setq v2 (org-entry-get nil "P_UUID")))

    (format "%-6s%-9s" v1 v2 )))
(defun my/org-agenda-pf-next-p ()
  "Used by org-agenda-custom-commands (init-org-agenda-mode.el)
   Function: return %-10c"
  (let* ((todo_keyword (org-get-todo-state))
	 (category (org-get-category))
	 (v ""))
    (when (string= todo_keyword "PROJECT") (setq v category))
    (when (string= todo_keyword "TODO"))
    (when (string= todo_keyword "WAITING") (setq v "[w]"))
    (format "%-10s" v)))
(defun my/org-agenda-pf-archive ()
  "Used by org-agenda-custom-commands (init-org-agenda-mode.el)
   Function: return Properties INIT_TODO value"
  (let ((todo_keyword (org-entry-get nil "CAPTURE_TODO"))
	(v ""))
    (when (string= todo_keyword "PROJECT")
      (setq v todo_keyword))
    (when (string= todo_keyword "TODO")) ; do nothing
    (when (string= todo_keyword "WAITING")
      (setq v todo_keyword))
    (format "%s" v)))

;; ****************************************************
;; 快速切换agenda buffer
;; ****************************************************
;; 标识当前agenda-buffer编号
(defvar my-org-agenda-buffer-no 1)
(setq set_org_buffer_no (lambda (val)
			  (setq my-org-agenda-buffer-no val)))

(defun my-is-inbox-have-headings()
  "check inbox.org if have something"
  (interactive)
  (with-current-buffer (find-buffer-visiting my-file-inbox)
    (goto-char (point-min))
    (re-search-forward "* " nil t)))

(defun my-org-agenda-forward ()
  "Used by user;
   Function: Move next buffer;"
  (interactive)
  (unless (derived-mode-p 'org-agenda-mode)
    (user-error "Can only append from within agenda buffer"))
  (let ((org-agenda-multi nil))
    (cond
     ;; agenda-view, next-view, inbox-view 互相滚动
     ((= my-org-agenda-buffer-no 1)
      (org-agenda nil "n"))
     ((= my-org-agenda-buffer-no 2)
      (if (my-is-inbox-have-headings)
	  (org-agenda nil "i")
	(org-agenda nil "a")))
     ((= my-org-agenda-buffer-no 3)
      (org-agenda nil "a"))
     ;; project view, archive view 互相滚动
     ((= my-org-agenda-buffer-no 4)
      (org-agenda nil "r"))
     ((= my-org-agenda-buffer-no 5)
      (org-agenda nil "p")))))

;; ****************************************************
;; 快速显示关闭 item
;; ****************************************************
(defvar my-last-show-entry "")
(defun my-org-agenda-show ()
  "Used in agenda-buffer by user;
   Fucntion: toggle the item show|hide "
  (interactive)
  (let ((current-show-entry (format "%S" (org-get-at-bol 'org-marker))))
    (when current-show-entry
      (if (string-equal my-last-show-entry current-show-entry)
	  (funcall (lambda ()
		     (setq my-last-show-entry "")
		     (org-agenda-goto t)
		     (delete-window)))
	(setq my-last-show-entry current-show-entry)
	(let ((win (selected-window)))
	  (org-agenda-goto t)
	  (org-show-entry)
	  (org-narrow-to-subtree)
	  (select-window win) )))))

(defun my-org-agenda-enter ()
  "Used in agenda-buffer by user;
   Fucntion: enter the item org-file"
  (interactive)
  ;; switch-to org-file
  (org-agenda-switch-to)

  (let ((pos (point)))
    ;; show contents, shift-tab
    (org-content)
    ;; narrow
    (org-narrow-to-subtree)
    ;;
    (goto-char pos)))

;; ****************************************************
;; 按r 刷新
;; ****************************************************
(defun my-org-agenda-redo ()
  "Used in agenda-buffer by user;
   Function: refresh agenda bufffer"
  (interactive)
  (my-org-refile-all-todo)             ;;refile entry which tag match PRORETY::P_UUID
  (my-org-archive-all-done)            ;;archive: TODONOW 带整理
  (org-save-all-org-buffers)           ;;
  (org-agenda-redo t))                 ;;call origin redo

(defun my-org-refile-all-todo ()
  "Used by org-agenda-redo
Function: refile all todo|project|waitting item;  inbox.org -> task.org"
  (with-current-buffer  (find-buffer-visiting my-file-inbox)
    ;; hide org-file all headings
    (goto-char (point-min))
    (org-show-all '(headings))
    (org-shifttab)
    ;; goto first line
    (goto-char (point-min))
    ;; lootp: next line
    (while (/= (point) (point-max))
      ;; tags非nil, 并且满足TODO的entry才需要refile
      (when (or (org-match-line "* TODO")
		(org-match-line "* WAITING")
		(org-match-line "* PROJECT")
		(org-match-line "* DONE")
		(org-match-line "* CANCEL"))
	(let* ((tags (org-get-tags)))
	  (if tags
	      (dolist (tag tags)
		(when tag
		  (dolist (file (org-agenda-files))
		    (let ((buf (find-buffer-visiting file)))
		      (when buf
			(let ((pos (save-excursion
				     (set-buffer (file-name-nondirectory file))
				     (widen)
				     (org-find-property "P_UUID" tag))))
			  ;; TODONOW how to break dolist
			  (when pos
			    ;; rm tags && refile
			    (org-toggle-tag tag 'off)
			    (org-refile nil nil (list nil file nil pos)))))))))
	    ;; refile no-tags-entry to task-file
	    (org-refile nil nil (list nil my-file-task nil nil)))))
      (forward-line))))

(defun my-org-archive-all-done (&optional tag)
  "Used by org-agenda-redo
  Function: archive all done item;  task.org -> trash.org"
  (with-current-buffer (find-buffer-visiting my-file-task)
    (goto-char (point-min))

    (my-org-archive-all-matches
     (lambda (_beg end)
       (let ((case-fold-search nil))
	 (unless (re-search-forward org-not-done-heading-regexp end t)
	   "no open TODO items")))
     tag)))
(defun my-org-archive-all-matches (predicate &optional tag)
  "same as org-archive-all-matches, accept not promt confirm y-or-n"
  (let ((rea (concat ".*:" org-archive-tag ":")) re1
	(begm (make-marker))
	(endm (make-marker))
	(question (if tag "Set ARCHIVE tag? "
		    "Move subtree to archive? "))
	reason beg end (cntarch 0))
    (if (org-at-heading-p)
	(progn
	  (setq re1 (concat "^" (regexp-quote
				 (make-string
				  (+ (- (match-end 0) (match-beginning 0) 1)
				     (if org-odd-levels-only 2 1))
				  ?*))
			    " "))
	  (move-marker begm (point))
	  (move-marker endm (org-end-of-subtree t)))
      (setq re1 "^* ")
      (move-marker begm (point-min))
      (move-marker endm (point-max)))
    (save-excursion
      (goto-char begm)
      (while (re-search-forward re1 endm t)
	(setq beg (match-beginning 0)
	      end (save-excursion (org-end-of-subtree t) (point)))
	(goto-char beg)
	(if (not (setq reason (funcall predicate beg end)))
	    (goto-char end)
	  (goto-char beg)
	  (if (and (or (not tag) (not (looking-at rea)))
		   t)
	      (progn
		(if tag
		    (org-toggle-tag org-archive-tag 'on)
		  (org-archive-subtree))
		(setq cntarch (1+ cntarch)))
	    (goto-char end)))))
    (message "%d trees archived" cntarch)))

;; ****************************************************
;; 对agenda-file 进行排序
;; ****************************************************
(defun my-org-sort-entries (file)
  "Used by my/org-agenda-hook in org-agenda-mode.el
   Function: sort agenda entries"
  (let ((buf (find-buffer-visiting file)))
    (when buf
      (with-current-buffer buf
	(goto-char (point-min))
	;; check if file has heading, avoid 'org-sort-entries' put 'user-error'
	(or (org-at-heading-p) (outline-next-heading))
	(when (org-at-heading-p)
	  (goto-char (point-min))
	  (org-sort-entries t ?a) ;sort by alpha

	  (goto-char (point-min))
	  (org-sort-entries t ?p) ;sort by proprity

	  (goto-char (point-min))
	  (org-sort-entries nil ?o) ) ;sort by TODO KEY
	))))

;; ****************************************************
;; org-agenda-day|week|month|year-view 切换上一个与下一个
;; ****************************************************
(defun my/org-agenda-dwmy-view-previous ()
  "Used by init-local-shortkey
   Goto previous day|week|month|year"
  (interactive)
  ;; 仅在org agenda下生效
  (org-agenda-check-type t 'agenda)
  (let* ((args (get-text-property (min (1- (point-max)) (point)) 'org-last-args))
	 (curspan (nth 2 args))) ;; get agenda type (day, week, month, year)
    (let* ((sd (or (org-get-at-bol 'day)
		   (nth 1 args)
		   org-starting-day))
	   ;; get starting-day (day:today, week, month, year:第一天)
	   (sd (org-agenda-compute-starting-span sd curspan ))
	   ;; get list (month, day , year)
	   (greg (calendar-gregorian-from-absolute sd))
	   (mg (car greg)) ; current-month
	   (yg (nth 2 greg)) );current-year

      ;; 获得需要的sd
      (cond
       ((eq curspan 'day)
	;; day只需要+1就是明天, -1就是昨天
	(setq sd (- sd 1)))
       ((eq curspan 'week)
	;; week只需要+7就是下周, -7就是上周
	(setq sd (- sd 7)))
       ((eq curspan 'month)
	;; month不固定天数, 因此这里需要计算
	(when (< (- mg 1) 1)
	  (setq mg 13)
	  (setq yg (- yg 1)))
	(setq sd (calendar-absolute-from-gregorian (list (- mg 1) 1 yg))))
       ((eq curspan 'year)
	(setq sd (calendar-absolute-from-gregorian (list 1 1 (- yg 1))))))

      (let ((org-agenda-overriding-arguments (list (car args) sd curspan)))
	(org-agenda-redo) ))))

(defun my/org-agenda-dwmy-view-next ()
  "Used by init-local-shortkey
   Goto next day|week|month|year"
  (interactive)
  ;; 仅在org agenda下生效
  (org-agenda-check-type t 'agenda)
  (let* ((args (get-text-property (min (1- (point-max)) (point)) 'org-last-args))
	 (curspan (nth 2 args))) ;; get agenda type (day, week, month, year)
    (let* ((sd (or (org-get-at-bol 'day)
		   (nth 1 args)
		   org-starting-day))
	   ;; get starting-day (day:today, week, month, year:第一天)
	   (sd (org-agenda-compute-starting-span sd curspan ))
	   ;; get list (month, day , year)
	   (greg (calendar-gregorian-from-absolute sd))
	   (mg (car greg)) ; current-month
	   (yg (nth 2 greg)) );current-year

      ;; 获得需要的sd
      (cond
       ((eq curspan 'day)
	;; day只需要+1就是明天, -1就是昨天
	(setq sd (+ sd 1)))
       ((eq curspan 'week)
	;; week只需要+7就是下周, -7就是上周
	(setq sd (+ sd 7)))
       ((eq curspan 'month)
	;; month不固定天数, 因此这里需要计算
	(when (> (+ mg 1) 12)
	  (setq mg 0)
	  (setq yg (+ yg 1)))
	(setq sd (calendar-absolute-from-gregorian (list (+ 1 mg) 1 yg))))
       ((eq curspan 'year)
	(setq sd (calendar-absolute-from-gregorian (list 1 1 (+ yg 1))))))

      (let ((org-agenda-overriding-arguments (list (car args) sd curspan)))
	(org-agenda-redo) ))))

(provide 'init-org-agenda-fun)
