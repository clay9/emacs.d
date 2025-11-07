;;; init-text-snippet.el --- Snippet settings -*- lexical-binding: t -*-
;;; Commentary:
;; 配置 Emacs 的 snippet 系统，基于 yasnippet。
;; - 全局启用 yasnippet
;; - 自定义 Hugo 文章 front-matter 相关函数，便于 snippet 自动填充
;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  ;; 设置 snippet 存放路径
  (setq yas-snippet-dirs (list (expand-file-name ".yasnippet" user-emacs-directory)))

  ;; ------------------------------------------------------------
  ;;; Hugo 文章 snippet 辅助函数
  ;; ------------------------------------------------------------

  ;; 获取 Hugo bundle 路径（相对路径）
  ;; 例如:
  ;;   ~/blog/content/post/org/foo/_index.org -> "post/foo"
  ;;   ~/blog/content/post/org/foo/bar.org    -> "post/foo/bar"
  (defun yas/hugo-get-bundle ()
    "Return Hugo content bundle path for current file."
    (let* ((file-path (expand-file-name (buffer-file-name)))
           (file-dir (file-name-directory file-path))
           (index (string-match "org/" file-dir))
           (rpath (if index
                      (substring file-dir (+ index 4) -1)
                    "")) ;; 如果找不到 \"org/\"，返回空
           (file-name (file-name-sans-extension (file-name-nondirectory file-path))))
      (if (string= file-name "_index")
          rpath
        (concat rpath "/" file-name))))

  ;; 获取 Hugo bundle 文件名
  ;; - 如果是 _index.org -> "_index"
  ;; - 否则 -> "index"
  (defun yas/hugo-get-bundle-name ()
    "Return Hugo bundle name (_index or index)."
    (let* ((file-path (expand-file-name (buffer-file-name)))
           (file-name (file-name-sans-extension (file-name-nondirectory file-path))))
      (if (string= file-name "_index")
          "_index"
        "index")))

  ;; 获取Hugo weight
  ;; - 如果是 11_xxx.org -> 11
  ;; - 否则 -> nil
  (defun yas/hugo-get-weight()
    "Return leading numeric weight from current filename like 11_xxx.org."
    (let ((fname (buffer-file-name)))
      (when fname
        (let ((base (file-name-nondirectory fname)))
          (when (string-match "\\`\\([0-9]+\\)_" base)
            (string-to-number (match-string 1 base)))))))

  ;; 获取 Hugo front-matter 中 collapse 属性
  ;; - _index.org -> "true"
  ;; - 否则 -> "false"
  (defun yas/hugo-get-collapse-section ()
    "Return \"true\" if file is _index, otherwise \"false\"."
    (let* ((file-path (expand-file-name (buffer-file-name)))
           (file-name (file-name-sans-extension (file-name-nondirectory file-path))))
      (if (string= file-name "_index")
          "true"
        "false")))

  ;; ------------------------------------------------------------
  ;;; 全局启用 yasnippet
  ;; ------------------------------------------------------------
  (yas-global-mode t))

(provide 'init-text-snippet)
;;; init-text-snippet.el ends here
