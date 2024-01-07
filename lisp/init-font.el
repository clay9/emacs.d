;;; init-font.el --- font  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; mac os
(when (eq system-type 'darwin)
  ;; set English Font
  (set-face-attribute 'default nil :font "Monaco 14")

  ;; set Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (when window-system
      (set-fontset-font (frame-parameter nil 'font)
			charset
			(font-spec :family "冬青黑体简体中文 W3" :size 16)))))


;; linux
(when (eq system-type 'gnu/linux)
  ;; set English Font
  (set-face-attribute 'default nil :font "DejaVu Sans Mono 12")

  ;; set Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (when window-system
      (set-fontset-font (frame-parameter nil 'font)
			charset
			(font-spec :family "楷体" :size 20)))))


;; windows
(when (eq system-type 'windows-nt)
  ;; set English Font
  (set-face-attribute 'default nil :font "Monaco 10")

  ;; set Chinese Font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (when window-system
      (set-fontset-font (frame-parameter nil 'font)
			charset
			(font-spec :family "Microsoft Yahei" :size 20)))))


(provide 'init-font)
;;; init-font.el ends here
