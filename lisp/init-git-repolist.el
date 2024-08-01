;;; init-git-repolist.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package magit
  :commands (transient/magit-list-repos)
  :bind ( :map magit-repolist-mode-map
          ("SPC" . magit-repos/switch)
          ("C-j" . transient/magit-repolist))
  :config
  (setq magit-repolist-my-repos-my
        '(("clay9/emacs.d"           "~/.emacs.d")
          ("clay9/blog"              "~/my/blog")
          ("clay9/gtd"               "~/my/gtd")))
  (setq magit-repolist-my-repos-qy
        '(("qygame/blog"             "~/qy/blog")
          ("qygame/build_publish"    "~/qy/ops/build_publish")
          ("qygame/compiler"         "~/qy/ops/compiler")
          ("qygame/docker_compose"   "~/qy/ops/docker_compose")
          ;; ("qygame/k8s"              "~/qy/ops/k8s")
          ("qygame/svr-kernel"       "~/qy/server-kernel")
          ("qygame/server"           "~/qy/server")
          ("qygame/database"         "~/qy/database")
          ;;("qygame/client"           "~/qy/client")
          ))

  (setq magit-repolist-index 0)
  (defun magit-repos/get-repos()
    (cl-case magit-repolist-index
      (0 magit-repolist-my-repos-my)
      (1 magit-repolist-my-repos-qy)))
  (defun reset-magit-repository-directories()
    (let* ((new-list ))
      (dolist (v (magit-repos/get-repos))
        (add-to-list 'new-list (cons (cadr v) 0)))
      new-list))

  :config
  (transient-define-prefix transient/magit-repolist()
    [["fetch"
      ("f" "pull" magit-repolist/magit-repolist-fetch)
      ("F" "Pull all" magit-repolist/magit-repolist-fetch-all)]
     ["push"
      ("p" "push" magit-repolist/magit-repolist-push)]
     ["clone"
      ("c" "clone all" magit-repolist/magit-repolist-clone)]])
  (defun magit-repos/switch ()
    (interactive)
    (cl-case magit-repolist-index
      (0 (setq magit-repolist-index 1) (rename-buffer "repos qy"))
      (1 (setq magit-repolist-index 0) (rename-buffer "repos my")))
    (setq magit-repository-directories (reset-magit-repository-directories))
    (revert-buffer))
  (defun magit-repolist/magit-repolist-fetch()
    (interactive)
    (run-hooks 'magit-credential-hook)
    (let* ((repo (tabulated-list-get-id))
           (default-directory (file-name-as-directory repo))
           (msg (format "Pull in %s" default-directory)))
      (message msg)
      (magit-call-git "pull")
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
               (msg (format "(%s/%s) Pull in %s..."
                            (cl-incf i) l default-directory)))
          (message msg)
          (magit-call-git "pull")
          (message (concat msg "  done")))))
    (magit-refresh-buffer))
  (defun magit-repolist/magit-repolist-push()
    (interactive)
    (run-hooks 'magit-credential-hook)
    (let* ((repo (tabulated-list-get-id))
           (default-directory (file-name-as-directory repo))
           (msg (format "Push %s" repo)))
      (message msg)
      (magit-call-git "push")
      (message (concat msg "  done")))
    (magit-refresh-buffer))
  (defun magit-repolist/magit-repolist-clone()
    (interactive)
    (run-hooks 'magit-credential-hook)
    (let* ((magit-clone-set-remote.pushDefault nil))
      (dolist (v (magit-repos/get-repos))
        (let* ((url (magit-clone--name-to-url (car v)))
               (path (cadr v))
               (args (caddr v)))
          (unless (file-exists-p path)
            (magit-run-git-async "clone" args "--" url
                                 (magit-convert-filename-for-git path)))))))

  (defun transient/magit-list-repos ()
    ;; 1. if buffer "repos *" exist, reuse it
    ;; 2. or call magit-list-repositories
    ;;    TODO: also set index=0 && rename-buffer "repos my"
    (interactive)
    (let ((buff_already_exist nil))
      (dolist (buf '("repos my" "repos qy"))
        (when (and (get-buffer buf) (not buff_already_exist))
          (setq buff_already_exist t)
          (switch-to-buffer buf)))
      (when (not buff_already_exist)
        (magit-list-repositories))))

  :config
  (setq magit-repository-directories (reset-magit-repository-directories))
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
          (format "%s (%s)" tag cnt))))))

(use-package magit
  :config
  ;;; my/gtd: auto pull && auto push

  ;; (add-hook 'after-init-hook #'(lambda ()
  ;;                                (let* ((default-directory (file-name-as-directory "~/my/gtd")))
  ;;                                  (when default-directory
  ;;                                    (when (magit-anything-modified-p)
  ;;                                      (magit-call-git "commit" "-am auto-commit"))
  ;;                                    (magit-call-git "fetch")
  ;;                                    (let* ((br (magit-get-upstream-branch))
  ;;                                           (unpull-count (cadr (magit-rev-diff-count "HEAD" br))))
  ;;                                      (when (> unpull-count 0)
  ;;                                        (magit-call-git "rebase")))))))
  (add-hook 'kill-emacs-hook #'(lambda ()
                                 (let* ((default-directory (file-name-as-directory "~/my/gtd")))
                                   (when default-directory
                                     (when (magit-anything-modified-p)
                                       (magit-call-git "commit" "-am auto-push"))
                                     (let* ((br (magit-get-upstream-branch))
                                            (unpush-count (car (magit-rev-diff-count "HEAD" br))))
                                       (when (> unpush-count 0)
                                         (magit-call-git "push"))))))))

(provide 'init-git-repolist)
;;; init-git-repolist.el ends here
