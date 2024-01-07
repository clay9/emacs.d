;;; init-gdb.el --- Settings   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'gdb-mi)
(require 'speedbar)

(setq gdb-many-windows t)
(setq gdb-speedbar-auto-raise t)

;;; defun
(defun gdb-setup-windows ()
  "Layout the window pattern for option `gdb-many-windows'."
  (gdb-get-buffer-create 'gdb-locals-buffer)
  (gdb-get-buffer-create 'gdb-stack-buffer)
  (gdb-get-buffer-create 'gdb-breakpoints-buffer)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  ;;关闭speedbar -- 非常容易导致界面混乱
  (delete-other-windows)
  (let ((win0 (selected-window))
	(win1 (split-window nil ( / ( * (window-height) 3) 4)))
	(win2 (split-window nil ( / (window-height) 3)))
	(win3 (split-window-right)))
    ;;1右 添加local buffer
    (gdb-set-window-buffer (gdb-locals-buffer-name) nil win3)
    (select-window win2)
    ;;2 添加source buffer
    (set-window-buffer
     win2
     (if gud-last-last-frame
	 (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
	   (gud-find-file gdb-main-file)
	 ;; Put buffer list in window if we
	 ;; can't find a source file.
	 (list-buffers-noselect))))
    (setq gdb-source-window (selected-window))
    ;;2右 添加io buffer
    ;(let ((win4 (split-window-right)))
    ;(gdb-set-window-buffer
    ;(gdb-get-buffer-create 'gdb-inferior-io) nil win4))
    ;;2右 添加speedbar -- gud watch使用
;    (let ((sr-speedbar-right-side t)
;   (sr-speedbar-width (/ (* (window-width) 1) 4))
;   (sr-speedbar-max-width (/ (* (window-width) 1) 4))
;   (sr-speedbar-default-width (/ (* (window-width) 1) 4)))
;      (sr-speedbar-open))
    ;;3 添加stack buffer
    (select-window win1)
    (gdb-set-window-buffer (gdb-stack-buffer-name))
    ;;3右 添加breakpoint buffer
    (let ((win5 (split-window-right)))
      (gdb-set-window-buffer (if gdb-show-threads-by-default
				 (gdb-threads-buffer-name)
			       (gdb-breakpoints-buffer-name))
			     nil win5))
    (select-window win0)))
(defun gud-kill ()
  "Kill gdb process, not target process"
  (interactive)
  (with-current-buffer gud-comint-buffer (comint-skip-input))
  (kill-process (get-buffer-process gud-comint-buffer))
  ;(kill-buffer "*gud\\*") ;关闭gud frame

  (org-agenda-list) ;显示agenda buffer
  (delete-other-windows))


(provide 'init-gdb)
;;; init-gdb.el ends here
