;;; init-corfu.el --- Buffer completion system -*- lexical-binding: t -*-
;;; Commentary:
;; Buffer 内的补全系统：
;; - corfu: inline completion UI
;; - corfu-terminal: 终端下的支持
;;; Code:

;; ------------------------------------------------------------
;;; Corfu: buffer 内补全
;; ------------------------------------------------------------
(use-package corfu
  :custom
  (corfu-auto t) ;; 自动补全
  :init
  (global-corfu-mode))

;; ------------------------------------------------------------
;;; Corfu in terminal
;; ------------------------------------------------------------
(use-package corfu-terminal
  :ensure nil
  :if (not (display-graphic-p))
  :config
  ;; 启用corfu-terminal
  (corfu-terminal-mode +1)
  ;; 调整终端下的Corfu face
  (set-face-attribute 'corfu-default nil
                      :foreground "color-240"))

(provide 'init-corfu)
;;; init-corfu.el ends here
