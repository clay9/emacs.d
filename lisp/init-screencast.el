;;; init-screencast.el --- screen cast -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; 按键显示
(use-package keycast
  :commands keycast-tab-bar-mode)


;; 录屏 => gif动图.
;; 录屏=> 裁剪 => 转换gif => 压缩gif
;; MacOS:
;; 录屏软件 screencapture (系统自带)
;; 裁剪软件 mogrify(需要下载);  macos不需要裁剪, screencapture自己做的更好
;; 转换软件 convert (不确定是否需要下载了)
;; 压缩软件 gifsicle (需要下载)
(use-package gif-screencast
  :commands myy/screencast-toggle
  :ensure-system-package screencapture
  :config
  (setq gif-screencast-args (list "-x" "-o" (format "-l%s" (shell-command-to-string "osascript -e 'tell app \"Emacs\" to id of window 1'"))))
  (setq gif-screencast-cropping-program "nil") ;;不要裁剪, screencapture自己做的更好
  (setq gif-screencast-capture-format "ppm")

  (setq gif-screencast-output-directory (expand-file-name "Movies/emacs/" "~"))

  (defun myy/screencast-toggle()
    (interactive)
    (if gif-screencast-mode
	(progn (gif-screencast-stop)
	       (keycast-tab-bar-mode 0))
      (gif-screencast)
      (let ((tab-bar-format (list 'tab-bar-format-align-right)))
        (keycast-tab-bar-mode 1)))))

(provide 'init-screencast)
;;; init-screencast.el ends here
