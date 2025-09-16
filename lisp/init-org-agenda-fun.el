;;; init-org-agenda-fun.el --- fun for org-agenda -*- lexical-binding: t -*-
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


;; Get org-agenda item property
(defun my/org-agenda-get-property (POM PROPERTY)
  "Get a property for the current headline."
  (interactive)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
		       (org-agenda-error)))
	 (buffer (marker-buffer hdmarker))
	 (pos (marker-position hdmarker))
	 (inhibit-read-only t)
         (result))
    ;; newhead
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-fold-show-context 'agenda)
        (setq result (org-entry-get POM PROPERTY))))
    result))


;; Get timestamp string
(defun my/org-timestamp-string (DAY &optional WITH-HM)
  "Get current timestamp string."
  (with-temp-buffer
    (org-mode)
    (org-insert-time-stamp (current-time) WITH-HM t)
    (when (/= DAY 0) (org-timestamp-up-day DAY))
    (buffer-substring (point-min) (point-max))))


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


;;; org-agenda-entry-text-mode => org-agenda-entry-text-show for single entry

(defvar my/org-agenda-entry-text-show-p nil)
(defun my/org-agenda-entry-text-show()
  (interactive)
  (save-excursion
    (let ((org-agenda-entry-text-maxlines 100)
          (org-agenda-entry-text-leaders nil)
          (show-p (my/org-agenda-entry-text-hide)))
      (when (and show-p
                 (org-get-at-bol 'org-hd-marker))
        (org-agenda-entry-text-show-here)))))
(defun my/org-agenda-entry-text-hide ()
  "Remove current shown entry context."
  (let ((res t))
    (mapc (lambda (o)
	    (when (eq (overlay-get o 'org-overlay-type)
		      'agenda-entry-content)
              (setq res nil)
	      (delete-overlay o)))
          (overlays-at (point)))
    res))


;;; skip entry

(defun my/org-agenda-skip-entry()
  "Skip entry when its root-parent todo-state is CANCEL or DONE."
  (org-back-to-heading t)
  (let* ((end (org-entry-end-position)))
    (if (my/org-entry-root-head-done)
        end
      nil)))
(defun my/org-entry-root-head-done ()
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


;;; 返回agenda (n) 中 item的前缀信息

;; defun org-agenda-prefix-format

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


;;; 快速切换agenda buffer

;; 标识当前agenda-buffer编号

(defvar my/org-agenda-buffer-number 1)
(defun my/org-agenda-set-buffer-number(number)
  (setq my/org-agenda-buffer-number number))

(defun my/org-agenda-empty-p()
  (with-current-buffer org-agenda-buffer
    (unless (next-single-property-change (line-end-position) 'org-marker)
      (org-agenda nil "n")))
  (remove-hook 'org-agenda-finalize-hook 'my/org-agenda-empty-p))
(defun my/is-inbox-have-headings()
  "Check inbox.org if have something."
  (with-current-buffer (find-buffer-visiting my/file-inbox)
    (goto-char (point-min))
    (re-search-forward "* " nil t)))
(defun my/org-agenda-forward ()
  "Used by user;
Function: Move next buffer;"
  (interactive)
  (unless (derived-mode-p 'org-agenda-mode)
    (user-error "Can only append from within agenda buffer"))
  (let ((org-agenda-multi nil))
    (cond
     ;; agenda-view, next-view, inbox-view 互相滚动
     ((= my/org-agenda-buffer-number 1)
      (org-agenda nil "n"))
     ((= my/org-agenda-buffer-number 2)
      (if (my/is-inbox-have-headings)
	  (org-agenda nil "i")
        (add-hook 'org-agenda-finalize-hook 'my/org-agenda-empty-p)
        (org-agenda nil "a")))
     ((= my/org-agenda-buffer-number 3)
      (add-hook 'org-agenda-finalize-hook 'my/org-agenda-empty-p)
      (org-agenda nil "a")))))


;;; 快速显示关闭 item

(defvar my/last-show-entry "")
(defun my/org-agenda-show ()
  "Used in agenda-buffer by user;
Fucntion: toggle the item show|hide"
  (interactive)
  (let ((current-show-entry (format "%S" (org-get-at-bol 'org-marker))))
    (when current-show-entry
      (let ((win (selected-window)))
        (if (string-equal my/last-show-entry current-show-entry)
	    (funcall (lambda ()
		     (setq my/last-show-entry "")
		     (org-agenda-goto)
                     (previous-buffer)
                     (select-window win)))
	  (setq my/last-show-entry current-show-entry)
	  (org-agenda-goto)
	  (org-show-entry)
	  (org-narrow-to-subtree)
	  (select-window win))))))
(defun my/org-agenda-enter ()
  "Used in agenda-buffer by user;
Fucntion: enter the item org-file"
  (interactive)
  (org-agenda-goto)
  (org-show-entry)
  (org-narrow-to-subtree)
  (kill-buffer org-agenda-buffer))


;;; 按r refresh buffer

(defun my/org-agenda-redo ()
  "Used in agenda-buffer by user;
Function: refresh agenda bufffer"
  (interactive)
  ;;file: inbox.org => task.org, gtd/xxx.org
  (my/org-refile-file-inbox)
  ;;save
  (no-message 'org-save-all-org-buffers)
  ;;call origin redo
  (no-message 'org-agenda-redo t))
(defun my/org-refile-file-inbox()
  "Refile todo-state heading in inbox.org.
1. Has tags => gtd/xxx.org. when enty with todo-state 'PROJECT && enty's 'ITEM = tag
2. No  tags => task.org"
  (with-current-buffer  (find-buffer-visiting my/file-inbox)
    ;; show level 1 headings
    (widen)
    (org-fold-show-all '(headings))
    (no-message 'org-shifttab 1)
    (goto-char (point-min))

    ;; loop: next todo-state heading
    (while (and (= 0 (org-next-visible-heading 1))
                (org-get-todo-state))
      ;; refile: heading with tags => heading;  others => task.org
      (let* ((tags (org-get-tags)))
	(if (not tags) (org-refile nil nil (list nil my/file-task nil nil))
	  (dolist (tag tags)
	    (dolist (file my/org-agenda-no-common-files)
              (let ((pos (with-current-buffer (find-buffer-visiting file)
                           (widen)
                           (org-fold-show-all '(headings))
                           (no-message 'org-shifttab 1)
                           (goto-char (point-min))
                           (let ((p nil))
                             (while (= 0 (org-next-visible-heading 1))
                               (when (and (string= "PROJECT" (org-get-todo-state))
                                          (string= tag (org-entry-get nil "ITEM")))
                                 (setq p (point))))
                             p))))
		(when pos
                  ;; TODONOW break loop

		  ;; rm tags && refile
		  (org-toggle-tag tag 'off)
		  (org-refile nil nil (list nil file nil pos)))))))))))


;;; org-agenda-day|week|month|year-view 切换上一个与下一个

(defun my/org-agenda-dwmy-view-previous()
  "Used by file init-local-shortkey, Goto previous day|week|month|year."
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
  "Used by init-local-shortkey, Goto next day|week|month|year."
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
;;; init-org-agenda-fun.el ends here
