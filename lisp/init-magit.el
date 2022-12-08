(require-package 'magit)
(require 'magit)

;; save-rep buffers auto when call magit-cmd
(setq magit-save-repository-buffers 'dontask)

;; set magit buffer window display in curret-buffer-window
(setq magit-display-buffer-function
      (lambda (buffer)
	(display-buffer
	 buffer (if (and (derived-mode-p 'magit-mode)
			 (memq (with-current-buffer buffer major-mode)
			       '(magit-process-mode
				 magit-revision-mode
				 magit-diff-mode
				 magit-stash-mode
				 magit-status-mode
				 magit-status-mode)))
		    nil
		  '(display-buffer-same-window)))))


;; ****************************************************
;; git repo list
;; ****************************************************
(setq magit-repository-directories
      '( ("~/my/" . 1)
	 ("~/qy/" . 2)))

(setq magit-repolist-columns
      '(("Name"   16 magit-repolist-column-ident   nil)
	("Flag"   6  magit-repolist-column-flag    nil)
	("UnPush" 7  magit-repolist-column-unpushed-to-upstream   nil)
	("UnPull" 7  magit-repolist-column-unpulled-from-upstream nil)
	("Time"   12 my/magit-repolist-column-time nil)
	("Tag"    15 my/magit-repolist-column-tag  nil)
	("Path"   99 magit-repolist-column-path    nil)))
(defun my/magit-repolist-column-time (_)
  "Insert time of the repository's `HEAD' revision."
  (let* ((gittime (magit-git-string "show" "--no-patch" "--format=%cd"
				    "--date=format:%Y-%m-%d.%H:%M"))
	 (ymd (substring gittime 0 10))
	 (y   (substring ymd 0 4))
	 (tim (substring gittime 11))
	 (cur_ymd (format-time-string "%Y-%m-%d"))
	 (cur_y (substring cur_ymd 0 4))
	 v)
    ;; 年月日相等时候, 显示时间 (使用green来标注一下)
    ;; 同一年的, 显示月份和日
    ;; 不同年的, 显示年月日
    (if (string= ymd cur_ymd)
	(progn (setq v tim)
	       (magit--put-face 0 (length v) 'calendar-month-header v))
      (if (string= y cur_y)
	  (setq v (substring ymd 5))
	(setq v ymd)))
    v))
(defun my/magit-repolist-column-tag (_)
  "Insert a description of the repository's `HEAD' revision."
  (let* ((tag (magit-get-current-tag nil t))
	 (cnt (cadr tag))
	 (tag (car tag)))
    (if (not tag)
	nil
      (if (eq cnt 0)
	  (format "%s" tag)
	(format "%s (%s)" tag cnt)))))


;; ****************************************************
;; git status
;; ****************************************************
;; headers
(setq magit-status-headers-hook
      '(magit-insert-repo-header
	magit-insert-head-branch-header
	;;magit-insert-user-header
	magit-insert-remote-header
	magit-insert-tags-header
	magit-insert-error-header
	magit-insert-diff-filter-header
	;magit-insert-upstream-branch-header
	magit-insert-push-branch-header))

(setq magit-module-sections-nested t)

;; not show indicator
(setq magit-section-visibility-indicator nil)

;; section
(set-face-attribute 'magit-section-highlight nil
		    :extend t
		    :background (face-attribute 'default :background))
;; hunk heading
(set-face-attribute 'magit-diff-hunk-heading-highlight nil
		    :extend t
		    :foreground (face-attribute 'default :foreground)
		    :background (face-attribute 'default :background))
(set-face-attribute 'magit-diff-hunk-heading nil
		    :extend t
		    :foreground (face-attribute 'default :foreground)
		    :background (face-attribute 'default :background))


;; ****************************************************
;; git diff
;; ****************************************************
(setq magit-diff-paint-whitespace nil)

(provide 'init-magit)
