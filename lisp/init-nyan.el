(require-package 'nyan-mode)

;; open nyan mdoe
(nyan-mode t)

;; forbid animate 
(setq nyan-animate-nyancat nil)
;; forbid wavy trail
(setq nyan-wavy-trail nil)

;; choose default cat type for terminal
(setq nyan-cat-face-number 2)

;; when window-width less than this, dont't show cat
(setq nyan-minimum-window-width 30)

(provide 'init-nyan)
