;;; sub-input-method.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'url)
(require 'json)
(require 'seq)
(require 'cl-lib)

(defun download-librime ()
  "Download latest librime macOS universal binary."
  (interactive)

  (let* ((api-url
          (if (string= my/librime-version "latest")
              "https://api.github.com/repos/rime/librime/releases/latest"
            (format "https://api.github.com/repos/rime/librime/releases/tags/%s"
                    my/librime-version)))
         (output-file
          (expand-file-name "cache/librime.tar.bz2" my/config-dir))
         json-buffer
         json
         asset
         download-url)

    ;; 获取 GitHub API JSON
    (let ((url-request-extra-headers
           '(("User-Agent" . "Emacs-librime-downloader")))) ;; 防止github限流
      (setq json-buffer
            (url-retrieve-synchronously api-url)))

    (unless json-buffer
      (display-warning
       'librime
       "Cannot access GitHub API"
       :warning)
      (cl-return-from download-librime nil))

    ;; 解析 JSON
    (unwind-protect
        (with-current-buffer json-buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (setq json
                (json-parse-buffer
                 :object-type 'alist
                 :array-type 'list)))

      (when (buffer-live-p json-buffer)
        (kill-buffer json-buffer)))

    ;; 查找目标文件
    (setq asset
          (seq-find
           (lambda (x)
             (let ((name (alist-get 'name x)))
               (and name
                    (string-match-p "macOS" name)
                    (string-match-p "tar\\.bz2\\'" name)
                    (not (string-match-p "deps" name)))))
           (alist-get 'assets json)))

    (unless asset
      (display-warning
       'librime
       "Cannot find librime macOS universal package"
       :warning)
      (cl-return-from download-librime nil))

    ;; 获取下载 URL
    (setq download-url
          (alist-get 'browser_download_url asset))

    (message "Downloading: %s" download-url)

    (condition-case err
        (url-copy-file
         download-url
         output-file
         t)

      (error
       (display-warning
        'librime
        (format "Download failed: %s"
                (error-message-string err))
        :warning)
       (cl-return-from download-librime nil)))

    (message "Downloaded: %s" output-file)))

(defun extract-librime ()
  "Extract librime archive into cache/librime directory."
  (interactive)

  (let* ((archive
          (expand-file-name "cache/librime.tar.bz2" my/config-dir))
         (extract-dir
          (expand-file-name "cache/librime" my/config-dir)))

    (unless (file-exists-p archive)
      (display-warning
       'librime
       (format "Archive not found: %s" archive)
       :warning)
      (cl-return-from extract-librime nil))

    (make-directory extract-dir t)

    (let ((default-directory extract-dir))
      (unless
          (= 0
             (call-process
              "tar"
              nil
              "*librime-extract*"
              nil
              "-xjf"
              archive))

        (display-warning
         'librime
         "Failed to extract librime"
         :warning)
        (cl-return-from extract-librime nil)))

    ;; 删除压缩包
    (when (file-exists-p archive)
      (delete-file archive)
      (message "Removed archive: %s" archive))

    (message "Extracted to: %s" extract-dir)))

;; download && extract librime
(unless (file-directory-p my/librime-dir)
  (download-librime)
  (extract-librime))

(provide 'sub-input-method)
;;; sub-input-method.el ends here
