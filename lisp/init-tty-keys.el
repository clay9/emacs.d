;;; init-tty-keys.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (not (display-graphic-p))
  (define-key input-decode-map (quote [27 91 51 59 97 126]) (kbd "C-<return>"))
  (define-key input-decode-map (quote [27 91 51 59 98 126]) (kbd "C--"))
  (define-key input-decode-map (quote [27 91 51 59 99 126]) (kbd "C-="))
  (define-key input-decode-map (quote [27 91 51 59 100 126]) (kbd "C-<backspace>")))

(provide 'init-tty-keys)
;;; init-tty-keys.el ends here
