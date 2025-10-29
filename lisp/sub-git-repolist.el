;;; sub-git-repolist.el --- Manage Git repositories -*- lexical-binding: t -*-
;;; Commentary:
;; - Magit Repo List Configure
;; - Auto Push repo: "~/my/gtd"
;;; Code:

(use-package magit
  :commands (magit-list-repositories)
  :bind (:map magit-repolist-mode-map
              ("SPC" . magit-repos/switch)
              ("C" . magit-repolist/magit-repolist-clone))
  :config
  ;;----------------------------------------
  ;;; 我的仓库列表
  ;;----------------------------------------
  (setq magit-repolist-my-repos-my
        '(("clay9/emacs.d" "~/.emacs.d")
          ("clay9/blog"    "~/my/blog")
          ("clay9/gtd"     "~/my/gtd")))

  ;;----------------------------------------
  ;;; 企业/公司仓库列表
  ;;----------------------------------------
  (setq magit-repolist-my-repos-qy
        '(("qygame/blog"             "~/qy/blog")
          ("qygame/build_publish"    "~/qy/ops/build_publish")
          ("qygame/compiler"         "~/qy/ops/compiler")
          ("qygame/docker_compose"   "~/qy/ops/docker_compose")
          ("qygame/qykernel"         "~/qy/qykernel")
          ("qygame/server"           "~/qy/server")
          ("qygame/database"         "~/qy/database")))

  ;;----------------------------------------
  ;;; 切换仓库列表
  ;;----------------------------------------
  (setq magit-repolist-index 0)
  (defun magit-repos/get-repos ()
    (cl-case magit-repolist-index
      (0 magit-repolist-my-repos-my)
      (1 magit-repolist-my-repos-qy)))

  (defun reset-magit-repository-directories ()
    "Reset `magit-repository-directories` based on current repo list."
    (let ((new-list nil))
      (dolist (v (magit-repos/get-repos))
        (add-to-list 'new-list (cons (cadr v) 0)))
      new-list))

  (defun magit-repos/switch ()
    (interactive)
    (cl-case magit-repolist-index
      (0 (setq magit-repolist-index 1))
      (1 (setq magit-repolist-index 0)))
    (setq magit-repository-directories (reset-magit-repository-directories))
    (revert-buffer))

  ;;----------------------------------------
  ;;; Clone 仓库
  ;;----------------------------------------
  (defun magit-repolist/magit-repolist-clone ()
    (interactive)
    (run-hooks 'magit-credential-hook)
    (let ((magit-clone-set-remote.pushDefault nil))
      (dolist (v (magit-repos/get-repos))
        (let ((url (magit-clone--name-to-url (car v)))
              (path (cadr v))
              (args (caddr v)))
          (unless (file-exists-p path)
            (magit-run-git-async "clone" args "--" url
                                 (magit-convert-filename-for-git path)))))))

  ;;----------------------------------------
  ;;; 列显示设置
  ;;----------------------------------------
  (setq magit-repository-directories (reset-magit-repository-directories))
  (setq magit-repolist-columns
        '(("Name" 18 magit-repolist-column-ident nil)
          ("S"    3  magit-repolist-column-flag nil)
          ("B>U"  3  magit-repolist-column-unpushed-to-upstream nil)
          ("B<U"  3  magit-repolist-column-unpulled-from-upstream nil)
          ("Date" 12 magit-repos/magit-repolist-column-time nil)
          ("Tag"  8  magit-repos/magit-repolist-column-tag nil)
          ;; ("Path" 99 magit-repolist-column-path nil)
          ))

  ;;----------------------------------------
  ;;; 列格式化函数
  ;;----------------------------------------
  (defun magit-repos/magit-repolist-column-time (_)
    "Insert time of the repository's `HEAD' revision."
    (let* ((gittime (magit-git-string "show" "--no-patch" "--format=%cd" "--date=format:%Y-%m-%d.%H:%M"))
           (ymd (substring gittime 0 10))
           (y (substring ymd 0 4))
           (time (substring gittime 11))
           (cur-ymd (format-time-string "%Y-%m-%d"))
           (cur-y (substring cur-ymd 0 4))
           v)
      ;; 年月日相等时候, 显示时间 (使用green来标注一下)
      ;; 同一年的, 显示月份和日
      ;; 不同年的, 显示年月日
      (setq v (cond ((string= ymd cur-ymd) time)
                    ((string= y cur-y) (substring ymd 5))
                    (t ymd)))
      (when (string= ymd cur-ymd)
        (magit--put-face 0 (length v) 'calendar-month-header v))
      v))

  (defun magit-repos/magit-repolist-column-tag (_)
    "Insert description of the repository's `HEAD' revision."
    (let* ((tag (magit-get-current-tag nil t))
           (cnt (cadr tag))
           (tag (car tag)))
      (if (not tag) nil
        (if (eq cnt 0)
            (format "%s" tag)
          (format "%s (%s)" tag cnt))))))

;;----------------------------------------
;;; 自动提交和推送 GTD 仓库
;;----------------------------------------
(use-package magit
  :config
  (defun magit-repos/auto-push(dir &optional auto-commit)
    (let ((default-directory dir))
      (when default-directory
        (when (and auto-commit
                   (magit-anything-modified-p))
          (magit-call-git "commit" "-am auto-push"))
        (let* ((br (magit-get-upstream-branch))
               (unpush-count (car (magit-rev-diff-count "HEAD" br))))
          (when (> unpush-count 0)
            (magit-call-git "push"))))))

  (defun magit-repos/auto-push-all()
    (magit-repos/auto-push "~/.emacs.d")
    (magit-repos/auto-push "~/my/blog" t)
    (magit-repos/auto-push "~/my/gtd" t)
    (magit-repos/auto-push "~/qy/blog" t))

  ;; 每次 Emacs 空闲 10 分钟执行一次
  (run-with-idle-timer 600 t #'magit-repos/auto-push-all)
  ;; 每天 18:00 定时执行一次
  (run-at-time "18:00" 86400 #'magit-repos/auto-push-all))

(provide 'sub-git-repolist)
;;; sub-git-repolist.el ends here
