(require 'calendar)
(require 'cal-china)
(require-package 'cal-china-x)
(require 'cal-china-x)

;; ****************************************************
;; Calendar
;; ****************************************************
;; init-cal-china-x 中国节日
(setq calendar-holidays (append cal-china-x-chinese-holidays
				'((holiday-lunar 1 15 "元宵节"))))

;; set first day is monday
;;(setq calendar-week-start-day 1)
;;(setq mark-holidays-in-calendar t)

;; appt
;;(appt-activate t)
;;(setq appt-audible nil)
;;(setq appt-display-format (quote window))
;;(setq appt-display-duration 40)
;;(setq appt-display-interval 1)
;;(setq appt-message-warning-time 4)


;; ****************************************************
;; defun
;; ****************************************************
;; 写入农历日期 获取最近的对应的阳历日期
(defun my/lunar (lunar-month lunar-day)
  (let* ((current-month (car (calendar-current-date)))
	 (current-year (cadr (cdr (calendar-current-date))))
	 (cn-years (calendar-chinese-year ; calendar-chinese-year counts from 12 for last year
		    (if (and (eq current-month 12) (eq lunar-month 12))
			(1+ current-year)
		      current-year)))
	 (run-yue (assoc lunar-month cn-years))
	 (date (calendar-gregorian-from-absolute
		(+ (cadr run-yue) (1- lunar-day))))
	 )
    date))



(provide 'init-calendar)
