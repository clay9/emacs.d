;;; init-font.el --- font  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/set-chinese-font (font size)
  "Set FONT with SIZE for common CJK characters."
  (when window-system
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family font :size size)))))



(cond
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Monaco 18")
  (my/set-chinese-font "冬青黑体简体中文 W3" 22))

 ((eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :font "DejaVu Sans Mono 12")
  (my/set-chinese-font "楷体" 20))

 ((eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Monaco 10")
  (my/set-chinese-font "Microsoft Yahei" 20)))

(provide 'init-font)
;;; init-font.el ends here
