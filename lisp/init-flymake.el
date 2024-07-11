;;; init-flymake.el --- Configure Flymake global behaviour -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;(add-hook 'prog-mode-hook 'flymake-mode)


;;; backend: c,c++ format

(use-package flymake-easy)
(use-package flymake-google-cpplint
  :load-path "site-lisp/flymake-google-cpplint"
  :hook (c++-ts-mode . flymake-google-cpplint-load)
  :config
  (setq flymake-google-cpplint-command "cpplint"
        flymake-google-cpplint-verbose "--verbose=0"
        flymake-google-cpplint-filter "--filter=-legal/copyright,-build/header_guard,-build/include_subdir"))

(provide 'init-flymake)
;;; init-flymake.el ends here
