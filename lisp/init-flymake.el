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
(use-package flymake-easy :defer t)
(use-package flymake-google-cpplint
  :vc (:url "https://github.com/flymake/flymake-google-cpplint.git" :rev :newest)
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode) . flymake-google-cpplint-load)
  :init
  ;; 确保 cpplint 路径有效
  (setq flymake-google-cpplint-command
        (or (executable-find "cpplint.py")
            "/usr/local/bin/cpplint.py"))
  :config
  (setq flymake-google-cpplint-verbose "--verbose=0"
        flymake-google-cpplint-filter
        "--filter=-legal/copyright,-build/header_guard,-build/include_subdir"))

(provide 'init-flymake)
;;; init-flymake.el ends here
