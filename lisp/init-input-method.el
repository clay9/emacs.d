;;; init-input-method.el --- Input Method -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst my/librime-version "1.16.0" "can be `latest'")
(defconst my/librime-dir (expand-file-name "cache/librime/dist" my/config-dir) "Rime librime path")

(require 'sub-input-method) ;; Auto download librime

(use-package rime
  :if (file-directory-p my/librime-dir) ;; 仅在 librime 存在时加载
  :ensure t
  :bind (:map rime-mode-map
              ("C-`" . rime-inline-ascii))
  :custom
  ;; Compile librime-emacs.dylib
  (rime-librime-root my/librime-dir)
  (rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include")

  ;; Rime Configure Direcoty
  (rime-user-data-dir "~/my/gtd/.rime")

  ;; default method
  (default-input-method "rime")

  ;; 避免 Shift 被系统拦截  TODO wcq 2025/10/29 not work
  ;;(rime-inline-ascii-trigger 'shift-l)

  ;; display
  (rime-show-candidate 'posframe)
  (rime-title "👻")
  ;;(rime-title "")
  ;; (rime-show-preedit 'inline)

  :config
  ;; 自动切换中英. 当其中有任何一个断言的值不是 nil 时，会自动使用英文
  (setq rime-disable-predicates
        '(;; 任意英文字符后
          rime-predicate-after-ascii-char-p
          ;; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
          rime-predicate-prog-in-code-p
          ;; 当要输入的是符号时
          rime-predicate-current-input-punctuation-p
          ;; 将要输入的为大写字母时
          rime-predicate-current-uppercase-letter-p
          ;; 在中文字符且有空格之后
          rime-predicate-space-after-cc-p))

  ;; 自动启用
  (defun my/activate-input-method()
    (activate-input-method default-input-method))
  (add-hook 'text-mode-hook #'my/activate-input-method)
  (add-hook 'prog-mode-hook #'my/activate-input-method)

  ;; 每次 Emacs 空闲 5 分钟执行一次
  (run-with-idle-timer 300 t #'rime-sync))

;; 如果 librime 不存在，输出提示
(unless (file-directory-p my/librime-dir)
  (message "Rime not start: Not found librime"))


(provide 'init-input-method)
;;; init-input-method.el ends here
