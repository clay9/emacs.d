;;; sub-git-repolist.el --- Manage Git repositories -*- lexical-binding: t -*-
;;; Commentary:
;; - Magit Repo List Configure
;; - Auto Push repo: "~/my/gtd"
;;; Code:

;;----------------------------------------
;;; Repolist
;;----------------------------------------
(defvar magit/repos
  '(
    ;; =====================
    ;; Columns explanation:
    ;; column 1: GitHub repository address
    ;; column 2: Local path on your machine
    ;; column 3: Whether to auto-commit (t/nil)
    ;; =====================

    ;; my
    ("clay9/emacs.d"             "~/.emacs.d"                     )
    ("clay9/blog"                "~/my/blog"                     t)
    ("clay9/gtd"                 "~/my/gtd"                      t)

    ;; qygame
    ("qygame/blog"               "~/qy/blog"                     t)
    ("qygame/qycompiler-builder" "~/qy/ops/qycompiler-builder"    )
    ("qygame/docker_compose"     "~/qy/ops/docker_compose"        )
    ("qygame/qykernel"           "~/qy/qykernel"                  )
    ("qygame/server"             "~/qy/server"                    )
    ("qygame/database"           "~/qy/database"                  )))

;;----------------------------------------
;;; Auto Commit && Push
;;----------------------------------------
(use-package magit
  :config
  (defun magit/auto-push(dir &optional auto-commit)
    (let ((default-directory dir))
      (when default-directory
        (when (and auto-commit
                   (magit-anything-modified-p))
          (magit-call-git "commit" "-am Auto Push")
          (message "Auto Commit %s" dir))
        (let* ((br (magit-get-upstream-branch))
               (unpush-count (car (magit-rev-diff-count "HEAD" br))))
          (when (> unpush-count 0)
            (magit-call-git "push")
            (message "Auto Push %s" dir))))))

  (defun magit/auto-push-all()
    (dolist (v magit/repos)
      (let ((path (cadr v))
            (auto-commit (caddr v)))
        (magit/auto-push path auto-commit))))

  ;; 每次 Emacs 空闲 5 分钟执行一次
  (run-with-idle-timer 300 t #'magit/auto-push-all))

;;----------------------------------------
;;; Emacs关闭的时候, 检测一下仓库状态
;;----------------------------------------
(use-package magit
  :config
  (defun magit/check (dir)
    (when dir
      (let* ((default-directory dir)
             (modify (magit-anything-modified-p))
             (br (magit-get-upstream-branch))
             (diff (magit-rev-diff-count "HEAD" br))
             (unpush-count (if br (car diff) 0))
             (unpull-count (if br (cadr diff) 0))
             (status '()))
        (when modify (push "modified" status))
        (when (> unpush-count 0) (push (format "unpush(%d)" unpush-count) status))
        (when (> unpull-count 0) (push (format "unpull(%d)" unpull-count) status))
        (when status
          (list dir (mapconcat #'identity (reverse status) ", "))))))

  (defun magit/check-all ()
    "Check all repositories in `magit/repos` and display status messages."
    (let ((results '()))
      (dolist (v magit/repos)
        (let* ((path (cadr v))
               (msg  (magit/check path)))
          (when msg (push msg results))))

      (if (null results)
          t  ;; 没变化 → 允许退出
        (let* ((results (reverse results)) ;; 保持顺序
               (formatted
                (cl-loop for item in results
                         for idx from 1
                         collect (format "%2d) %-26s | %s"
                                         idx (car item) (cadr item))))
               (table-header (format "%-30s | %s" "Repository" "Status"))
               (table-sep    (make-string 50 ?-))
               (all-msg (mapconcat #'identity formatted "\n"))
               (output (format "%s\n%s\n%s" table-header table-sep all-msg)))
          (let* ((prompt (concat
                          (propertize (format "Found %d repos with changes:\n\n" (length results))
                                      'face 'warning)
                          output
                          "\n\n"
                          (propertize "1-9" 'face 'success) ": open repo   "
                          (propertize "y" 'face 'success) ": exit   "
                          (propertize "n" 'face 'warning) ": cancel exit"))
                 (input (read-char-choice prompt
                                          (append '(?y ?n)
                                                  (cl-loop for i from ?1 to ?9 collect i)))))
            (cond
             ;; y → 允许退出
             ((eq input ?y) t)
             ;; n → 取消退出
             ((eq input ?n) nil)
             ;; 数字 → 打开 repo, 不退出
             ((and (>= input ?0) (<= input ?9))
              (let* ((index (- input ?1))
                     (entry (nth index results)))
                (when entry (magit-status (car entry))))
              nil)
             ;; 其他 → 取消退出
             (t nil)))))))

  ;; 将退出检查挂到 Emacs 关闭之前
  (add-hook 'kill-emacs-query-functions #'magit/check-all))

;;----------------------------------------
;;; Utility Functions
;;----------------------------------------
(use-package magit
  :config
  ;; Clone 仓库
  (defun my/clone-repos ()
    (interactive)
    (run-hooks 'magit-credential-hook)
    (let ((magit-clone-set-remote.pushDefault nil))
      (dolist (v magit/repos)
        (let ((url (magit-clone--name-to-url (car v)))
              (path (cadr v))
              (args nil))
          (unless (file-exists-p path)
            (magit-run-git-async
             "clone" args "--" url
             (magit-convert-filename-for-git path))))))))


(provide 'sub-git-repolist)
;;; sub-git-repolist.el ends here
