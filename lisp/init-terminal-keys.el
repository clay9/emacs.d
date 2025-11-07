;;; init-terminal-keys.el --- Keybindings for terminal (non-graphical) Emacs -*- lexical-binding: t -*-
;;; Commentary:
;; This file defines keybindings for TTY (non-graphical) Emacs.
;; It maps certain escape sequences to Emacs key events.
;;; Code:

;; 流程说明:
;; 1. terminal中输入Ctrl+Enter等组合键
;; 2. 终端无法直接发送 Emacs 的按键事件.
;;    于是发送类似 [27 91 51 59 97 126] 的转义序列
;; 3. Emacs 通过 input-decode-map 转换
;;    define-key input-decode-map [27 91 51 59 97 126] (kbd "C-<return>")
;;    告诉 Emacs “遇到这个序列时，就当作 C-<return> 处理”
;; 4. Emacs 识别按键事件
(when (not (display-graphic-p))
  ;; Map terminal escape sequences to Emacs key events
  (define-key input-decode-map [27 91 51 59 97 126] (kbd "C-<return>"))
  (define-key input-decode-map [27 91 51 59 98 126] (kbd "C--"))
  (define-key input-decode-map [27 91 51 59 99 126] (kbd "C-="))
  (define-key input-decode-map [27 91 51 59 100 126] (kbd "C-<backspace>")))

(provide 'init-terminal-keys)
;;; init-terminal-keys.el ends here
