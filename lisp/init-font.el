;;; init-font.el --- Font settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Set default font and CJK font according to system
;;; Code:

(when (display-graphic-p)
  (let ((set-chinese-font
         (lambda (font size)
           "Set FONT with SIZE for common CJK characters."
           (dolist (charset '(kana han symbol cjk-misc bopomofo))
             (set-fontset-font (frame-parameter nil 'font)
                               charset
                               (font-spec :family font :size size))))))
    (cond
     ((eq system-type 'darwin)
      (set-face-attribute 'default nil :font "Fira Code 19")
      (funcall set-chinese-font "思源黑体" 24)
      (message "Fonts loaded: Fire Code + 思源黑体"))

     ((eq system-type 'gnu/linux)
      (set-face-attribute 'default nil :font "DejaVu Sans Mono 12")
      (funcall set-chinese-font "楷体" 20)
      (message "Fonts loaded: DejaVu Sans Mono + 楷体"))

     ((eq system-type 'windows-nt)
      (set-face-attribute 'default nil :font "Monaco 10")
      (funcall set-chinese-font "Microsoft Yahei" 20)
      (message "Fonts loaded: Monaco + Microsoft Yahei")))))

(provide 'init-font)
;;; init-font.el ends here
