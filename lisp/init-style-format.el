;;; init-style-format.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake-easy)

(require 'flymake-google-cpplint)

(setq flymake-google-cpplint-command "cpplint")
(setq flymake-google-cpplint-verbose "--verbose=0")
(setq flymake-google-cpplint-filter "--filter=-legal/copyright,-build/header_guard")

(add-hook 'c++-ts-mode-hook 'flymake-google-cpplint-load)


(provide 'init-style-format)
;;; init-style-format.el ends here
