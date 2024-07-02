;;; init-style-format.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake-easy)

(require 'flymake-google-cpplint)

(custom-set-variables
 '(flymake-google-cpplint-command "/home/clay/.local/bin/cpplint")
 '(flymake-google-cpplint-verbose "--verbose=0")
 '(flymake-google-cpplint-filter "--filter=-legal/copyright"))

(add-hook 'c++-ts-mode-hook 'flymake-google-cpplint-load)


(provide 'init-style-format)
;;; init-style-format.el ends here
