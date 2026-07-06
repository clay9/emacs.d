;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;  Global Flymake setup with Google cpplint backend.
;;; Code:

;;----------------------------------------
;;; 基础配置
;;----------------------------------------
(use-package flymake
  ;; :hook ((prog-mode . flymake-mode))
  :config
  ;; 让 Flymake 的诊断更清晰
  (setq flymake-no-changes-timeout 0.5
        flymake-start-on-flymake-mode t
        flymake-start-on-save-buffer t
        flymake-fringe-indicator-position nil)
  ;; Make Flymake's Emacs Lisp backend aware of the current load-path
  (setq elisp-flymake-byte-compile-load-path load-path))

;;----------------------------------------
;;; C/C++ 后端: Google cpplint
;;----------------------------------------
(require 'sub-flymake-cpp)

(provide 'init-flymake)
;;; init-flymake.el ends here
