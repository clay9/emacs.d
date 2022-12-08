(require-package 'hydra)
(require 'hydra) ;;使用hydra管理快捷键

;; 目录一栏
;; 1.helm-mode         2.helm-gtags
;; 3.hs-hide-mode      4.speedbar
;; 5.compilation-mode
;; 6.org-mode
;; 7.org-agenda-mode
;; 8.artist-mode
;; 9.magit-mode
;; 10. emacs-lisp-mode
;; 11. help-mode

;; ****************************************************
;; 1.helm-mode
;; ****************************************************
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ;make TAB work in terminal
(define-key helm-map (kbd "C-z")   'helm-select-action)
(define-key helm-map (kbd "C-o")   'helm-previous-source)


;; ****************************************************
;; 2.helm-gtags
;; ****************************************************
(defhydra hydra-helm-gtags (:color blue
			     :hint nil)
  "
^gtags^
^^^^^^-----------------------------------------------------------------
_j_: tag
_f_: rtag
_s_: symbol
_a_: goto tag
_h_: pop stack
"
 ("j" helm-gtags-find-tag)
 ("f" helm-gtags-find-rtag)
 ("s" helm-gtags-find-symbol)
 ("a" helm-gtags-select)
 ("h" helm-gtags-pop-stack))
(define-key helm-gtags-mode-map (kbd "C-j") 'hydra-helm-gtags/body)


;; ****************************************************
;; 3.hs-minor-mode
;; ****************************************************
(define-key hs-minor-mode-map (kbd "<backtab>") 'my-hs-shift-tab);;shift + tab


;; ****************************************************
;; 4.speedbar
;; ****************************************************
(define-key speedbar-mode-map (kbd "u") 'speedbar-up-directory);;移动至上层目录, 类似info


;; ****************************************************
;; 5.compilation-mode
;; ****************************************************
(define-key compilation-mode-map (kbd "n") 'compilation-next-error)
(define-key compilation-mode-map (kbd "p") 'compilation-previous-error)
(define-key compilation-mode-map (kbd "TAB") 'compilation-display-error)
(define-key compilation-mode-map (kbd "RET") 'compile-goto-error)


;; ****************************************************
;; 6.org-mode
;; ****************************************************
(defun my-org-indent-subtree ()
  "Used by hydra-org-mode"
  (interactive)
  (let ((point (point)))
    (org-with-limited-levels
     (cond ((org-at-heading-p))
	   ((org-before-first-heading-p) (user-error "Not in a subtree"))
	   (t (outline-previous-visible-heading 1) )))
    (let ((begin (org-element-property :begin (org-element-at-point)))
	  (end   (org-element-property :end   (org-element-at-point))))
      ;(message "%s, %s, point: %s" begin end point)
      (org-indent-region begin end)
      (goto-char point) )))

(defhydra hydra-org-mode (:color blue
				 :hint nil)
  "
^columns^          ^subtree^          ^add info^          ^link^          ^export^          ^gtd^
^^^^^^------------------------------------------------------------------------------------------------
_1_: view          _j_: indent        _SPC_: todo         _i_: i&e        _C-1_: ascill     _a_: archive
_2_: insert        _w_: o narrow      _RET_: tag          _o_: open       _C-2_: md         ^ ^
^ ^                _W_: c narrow      _p_: property       _b_: goback     _C-3_: html       ^ ^
^ ^                ^ ^                _-_: priority-      ^ ^             ^ ^               ^ ^
^ ^                ^ ^                _=_: priority+      ^ ^             ^ ^               ^ ^
^ ^                ^ ^                _t_: time in        ^ ^             ^ ^               ^ ^
^ ^                ^ ^                _T_: time ac        ^ ^             ^ ^               ^ ^
"
  ("1" org-columns)
  ("2" org-columns-insert-dblock)

  ("j" my-org-indent-subtree)
  ("w" org-narrow-to-subtree)
  ("W" widen)

  ("SPC" org-todo)
  ("RET" org-set-tags-command)
  ("p" org-set-property)
  ("-" org-priority-down)
  ("=" org-priority-up)
  ("t" org-time-stamp-inactive)
  ("T" org-time-stamp)
  ("e" org-set-effort)

  ("i" org-insert-link)
  ("o" org-open-at-point)
  ("b" org-mark-ring-goto)
  ;;
  ("C-1" org-ascii-export-to-ascii)
  ("C-2" org-md-export-to-markdown)
  ("C-3" org-html-export-to-html)
  ;;
  ("a" org-archive-subtree))
(define-key org-mode-map (kbd "C-j") 'hydra-org-mode/body)

;; heading forward && backward
(define-key org-mode-map (kbd "M-p") 'org-backward-heading-same-level)
(define-key org-mode-map (kbd "M-n") 'org-forward-heading-same-level)
;; edit block in special-buffer && leave special-buffer
(define-key org-mode-map (kbd "C-c j") 'org-edit-special)
(define-key org-src-mode-map (kbd "C-c j") 'org-edit-src-exit)


;; ****************************************************
;; 7.org-agenda-mode
;; ****************************************************
(require 'org-agenda)
(defhydra hydra-org-agenda-mode (:color blue
				    :hint nil)
  "
^view^          ^tag(a)^          ^timestamp^
^^^^^^-----------------------------------------------
_1_: (c)olum    _t_: (t)odo       _s_: schedule
_2_: (R)eport   _:_: (:)tag       _d_: deadline
_3_: (l)og      _-_: (-)priority  _SPC_: clock int
_4_: tags       _=_: (+)priority  _RET_: clock out
_5_: search     _p_: (P)roperty   _c_:   clock cancel
^ ^             _e_: (e)ffort     _g_:   clock go
"
  ("1" org-agenda-columns)
  ("2" org-agenda-clockreport-mode)
  ("3" (funcall (org-agenda-log-mode 'clockcheck)))
  ("4" org-search-view)
  ("5" org-tags-view)

  ("t" org-agenda-todo)
  (":" org-agenda-set-tags)
  ("-" org-agenda-priority-down)
  ("=" org-agenda-priority-up)
  ("p" org-agenda-set-property)
  ("e" org-agenda-set-effort)

  ("s" org-agenda-schedule)
  ("d" org-agenda-deadline)

  ("SPC" #'(lambda() (interactive)
	     (org-agenda-clock-in)
	     (my-org-agenda-redo)))
  ("RET" #'(lambda() (interactive)
	     (org-agenda-clock-out)
	     (my-org-agenda-redo)))
  ("c" org-agenda-clock-cancel)
  ("g" org-agenda-clock-goto))
(define-key org-agenda-mode-map (kbd "C-j") 'hydra-org-agenda-mode/body)

;;
(define-key org-agenda-mode-map (kbd "r") 'my-org-agenda-redo)         ;refresh
(define-key org-agenda-mode-map (kbd "c") 'org-agenda-columns)         ;columns view
(define-key org-agenda-mode-map (kbd "\\") 'org-agenda-filter)         ;filter
(define-key org-agenda-mode-map (kbd "<backspace>") 'org-agenda-filter-remove-all) ;unfilter.
(define-key org-agenda-mode-map (kbd "`") #'(lambda()                  ;显示所有effort < 15的task
					      (interactive)
					      (org-agenda-filter-remove-all)
					      ;; must set this val. or `r' will remove all filter
					      (setq org-agenda-effort-filter (list "+<0:15"))
					      (org-agenda-filter-apply org-agenda-effort-filter 'effort)))

;; TODO-KEY && tag && PROPERTY && Priority
(define-key org-agenda-mode-map (kbd "t") 'org-agenda-todo)            ;set TODO-KEY
(define-key org-agenda-mode-map (kbd ":") 'org-agenda-set-tags)        ;set tags
(define-key org-agenda-mode-map (kbd "P") 'org-agenda-set-property)    ;set property
(define-key org-agenda-mode-map (kbd "e") 'org-agenda-set-effort)      ;set property effort

;; choose item
(define-key org-agenda-mode-map (kbd "p") 'org-agenda-previous-item)   ;previous item
(define-key org-agenda-mode-map (kbd "n") 'org-agenda-next-item)       ;next item

;; choose agenda view
(define-key org-agenda-mode-map (kbd "SPC") 'my-org-agenda-forward)
(define-key org-agenda-mode-map (kbd "<spc>") 'my-org-agenda-forward)
(define-key org-agenda-mode-map (kbd "1") #'(lambda() (interactive) (org-agenda nil "a")))
(define-key org-agenda-mode-map (kbd "2") #'(lambda() (interactive) (org-agenda nil "n")))
(define-key org-agenda-mode-map (kbd "3") #'(lambda() (interactive) (org-agenda nil "i")))
(define-key org-agenda-mode-map (kbd "4") #'(lambda() (interactive) (org-agenda nil "p")))
(define-key org-agenda-mode-map (kbd "5") #'(lambda() (interactive) (org-agenda nil "r")))

;; entry show
(define-key org-agenda-mode-map (kbd "TAB") 'my-org-agenda-show)
(define-key org-agenda-mode-map (kbd "<tab>") 'my-org-agenda-show)
;; entry enter
(define-key org-agenda-mode-map (kbd "RET") 'my-org-agenda-enter)

;; ----------------------  only effect in org-agenda C-a a ----------------------
;; view
(define-key org-agenda-mode-map (kbd "R") 'org-agenda-clockreport-mode)
(define-key org-agenda-mode-map (kbd "l") #'(lambda() (interactive) (org-agenda-log-mode 'clockcheck)))

;; d w m y: 显示天,星期, 月;  W: 这周&&下周; C-p, C-n: Goto previous|next day|week|month|year
(define-key org-agenda-mode-map (kbd "d") #'(lambda() (interactive) (org-agenda-goto-today) (org-agenda-day-view)))
(define-key org-agenda-mode-map (kbd "w") 'org-agenda-week-view)
(define-key org-agenda-mode-map (kbd "W") 'org-agenda-fortnight-view)
(define-key org-agenda-mode-map (kbd "m") 'org-agenda-month-view)
(define-key org-agenda-mode-map (kbd "y") 'org-agenda-year-view)
(define-key org-agenda-mode-map (kbd "g") #'(lambda() (interactive) (org-agenda-check-type t 'agenda)
					      (call-interactively 'org-agenda-goto-date)))
(define-key org-agenda-mode-map (kbd "-") #'(lambda() (interactive)
					      (if (org-agenda-check-type nil 'agenda)
						  (my/org-agenda-dwmy-view-previous)
						(org-agenda-priority-down))))
(define-key org-agenda-mode-map (kbd "=") #'(lambda() (interactive)
					      (if (org-agenda-check-type nil 'agenda)
						  (my/org-agenda-dwmy-view-next)
					      (org-agenda-priority-up))))
;; ----------------------               end                ----------------------

;; stop origin keymap
(define-key org-agenda-mode-map (kbd "a") 'nil)                 ;停止使用org-agenda-bulk-action, 使用C-j代替
(define-key org-agenda-mode-map (kbd "M") 'nil)                 ;org-agenda-bulk-mark-all
(define-key org-agenda-mode-map (kbd "u") 'nil)                 ;org-agenda-bulk-unmark
(define-key org-agenda-mode-map (kbd "U") 'nil)                 ;org-agenda-bulk-unmark-all
(define-key org-agenda-mode-map (kbd "j") 'nil)                 ;org-agenda-goto-date
(define-key org-agenda-mode-map (kbd "/") 'nil)                 ;org-agenda-filter


;; ****************************************************
;; 8.artist-mode
;; ****************************************************
(define-key artist-mode-map (kbd "C-<return>") 'org-newline-and-indent)
(define-key artist-mode-map (kbd "C-k") 'org-kill-line)
(defhydra hydra-artist-mode (:color blue
				    :hint nil)
  "
^shape^          ^action^
^^^^^^---------------------------------------------------
_l_: 线          _w_: cut  rectangle
_p_: 自定义线    _k_: kill rectangle
_r_: 四边形      _y_: yank
_e_: 椭圆        ^ ^
"
  ("l" my-artist-mode-draw-line)
  ("p" my-artist-mode-draw-custom-line)
  ("r" my-artist-mode-draw-rectangle)
  ("e" my-artist-mode-draw-ellipse)
; ("y" artist-select-op-spray-can) ;;水雾
; ("t" artist-select-op-text-overwrite) ;;艺术字
; ("T" artist-select-op-text-see-thru)  ;; 艺术字
; ("f" artist-select-op-flood-fill) ;;洪水效果, 填充整个图
  ("w" my-artist-mode-cut-rectangle)
  ("k" my-artist-mode-erase-rectangle)
  ("y" my-artist-mode-paste))
(define-key artist-mode-map (kbd "C-j") 'hydra-artist-mode/body)


;; ****************************************************
;; 9.magit-mode
;; ****************************************************
(define-key magit-status-mode-map (kbd "TAB") 'magit-section-cycle)


;; ****************************************************
;; 10.emacs-lisp-mode
;; ****************************************************
(define-key emacs-lisp-mode-map (kbd "C-x C-e") 'eval-defun)


;; ****************************************************
;; 11.help-mode
;; ****************************************************
(define-key help-mode-map (kbd "<return>") #'(lambda() (interactive)
					       (windmove-display-same-window)
					       (push-button)))


(provide 'init-local-shortkey)
