;;; init-locales.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Set UTF-8 as the default locale and coding system for Emacs.
;;; Code:

(defun sanityinc/locale-var-encoding (v)
  "Return the encoding portion of the locale string V, or nil if missing."
  (when v
    (save-match-data
      (let ((case-fold-search t))
        (when (string-match "\\.\\([^.]*\\)\\'" v)
          (intern (downcase (match-string 1 v))))))))

;; 检查环境变量中的编码
(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (let ((encoding (sanityinc/locale-var-encoding (getenv varname))))
    (unless (memq encoding '(nil utf8 utf-8))
      (message "Warning: non-UTF8 encoding in environment variable %s may cause interop problems with this Emacs configuration." varname))))

;; 设置字符优先级为 Unicode
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; 设置 UTF-8 为默认编码
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; 非 Windows 系统设置选择缓冲区编码
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))

(provide 'init-locales)
;;; init-locales.el ends here
