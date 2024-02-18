;;; init-git-repolist.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package magit
  :commands (magit-list-repositories)
  :bind ( :map magit-repolist-mode-map
          ("f" . magit-repolist/magit-repolist-fetch)
          ("F" . magit-repolist/magit-repolist-fetch-all)
          ("P" . magit-repolist/magit-repolist-push)
          ("C" . magit-repolist/magit-repolist-clone))
  :config
  (setq magit-repolist-my-repos
        '(("clay9/emacs.d"            "~/.emacs.d")
          ("clay9/blog"               "~/my/blog")
          ("clay9/gtd"                "~/my/gtd")
          ;; company
          ("clay9/blog_company"      "~/my/blog_company")
          ("clay9/blog_company"      "~/my/nginx" ("--branch=gh-pages"))))
          ;; qydocker
          ;; ("qydocker/build_image_dev" . "~/qy/docker/build_image_dev")
          ;; ("qydocker/build_publish"   . "~/qy/docker/build_publish")
          ;; ("qydocker/docker_compose"  . "~/qy/docker/docker_compose")
          ;; ("qydocker/k8s"             . "~/qy/docker/k8s")
          ;; qygame
          ;; ("qygame/svr-kernel"        . "~/qy/server/kernel")
          ;; ("qygame/svr-client"        . "~/qy/server/client")
          ;; ("qygame/svr-db"            . "~/qy/server/db")
          ;; ("qygame/svr-gate"          . "~/qy/server/gate")          
          ;; ("qygame/svr-room"          . "~/qy/server/room")
          ;; ("qygame/svr-22"            . "~/qy/server/22")
          ;; ("qygame/database"          . "~/qy/database")))
  :config
  (setq magit-repository-directories
        (let* ((new-list ))
          (dolist (v magit-repolist-my-repos)
            (add-to-list 'new-list (cons (cadr v) 0)))
          new-list))
  (setq magit-repolist-columns
        '(("Name" 18 magit-repolist-column-ident  nil)
          ("S"    3  magit-repolist-column-flag   nil)
          ("B>U"  3  magit-repolist-column-unpushed-to-upstream  nil)
          ("B<U"  3  magit-repolist-column-unpulled-from-upstream ((:right-align t)))
          ("Date" 12 magit-repos/magit-repolist-column-time nil)
          ("Tag"  8  magit-repos/magit-repolist-column-tag  nil)
          ("Path" 99 magit-repolist-column-path nil)))

  (defun magit-repos/magit-repolist-column-time (_)
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
  (defun magit-repos/magit-repolist-column-tag (_)
    "Insert a description of the repository's `HEAD' revision."
    (let* ((tag (magit-get-current-tag nil t))
           (cnt (cadr tag))
           (tag (car tag)))
      (if (not tag)
          nil
        (if (eq cnt 0)
            (format "%s" tag)
          (format "%s (%s)" tag cnt)))))

  :config
  (defun magit-repolist/magit-repolist-fetch()
    (interactive)
    (run-hooks 'magit-credential-hook)
    (let* ((repo (tabulated-list-get-id))
           (default-directory (file-name-as-directory repo))
           (msg (format "Pull in %s" default-directory)))
      (message msg)
      (magit-run-git "pull")
      (message (concat msg "  done")))
    (magit-refresh-buffer))
  (defun magit-repolist/magit-repolist-fetch-all()
    (interactive)
    (run-hooks 'magit-credential-hook)
    (let* ((repolist (magit-list-repos))
           (l (length repolist))
           (i 0))
      (dolist (repo repolist)
        (let* ((default-directory (file-name-as-directory repo))
               (msg (format "(%s/%s) Fetch in %s..."
                            (cl-incf i) l default-directory)))
          (message msg)
          (magit-run-git "remote" "update")
          (message (concat msg "  done")))))
    (magit-refresh-buffer))
  (defun magit-repolist/magit-repolist-push()
    (interactive)
    (run-hooks 'magit-credential-hook)
    (let* ((repo (tabulated-list-get-id))
           (default-directory (file-name-as-directory repo))
           (msg (format "Push %s" repo)))
      (message msg)
      (magit-run-git "push")
      (message (concat msg "  done")))
    (magit-refresh-buffer))
  (defun magit-repolist/magit-repolist-clone()
    (interactive)
    (run-hooks 'magit-credential-hook)
    (let* ((magit-clone-set-remote.pushDefault nil))
      (dolist (v magit-repolist-my-repos)
        (let* ((url (magit-clone--name-to-url (car v)))
               (path (cadr v))
               (args (caddr v)))
          (unless (file-exists-p path)
            (magit-run-git-async "clone" args "--" url
                                 (magit-convert-filename-for-git path)))))))
  )


(provide 'init-git-repolist)
;;; init-git-repolist.el ends here
