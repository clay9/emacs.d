;; menu bar
(menu-bar-mode 0)

;; tool bar
(tool-bar-mode 0)

;; scroll bar
(when window-system
  (scroll-bar-mode 0))

;; mode line
(defun get-minor-mode-alist ()
  "Used in Var: mode-line-format
  Fucntion: remove some minor-mode-alist from mode-line"
  (dolist (var minor-mode-alist)
    (let ((first (format "%s" (car var))))
      (when (or (string= first "smartparens-mode")
		(string= first "disable-mouse-mode")
		(string= first "disable-mouse-global-mode")
		(string= first "hs-minor-mode")
		(string= first "helm-mode")
		(string= first "yasnippet-mode")
		(string= first "yas-minor-mode")
		(string= first "company-mode")
		(string= first "eldoc-mode")
		)
	(delete var minor-mode-alist))))
  (list minor-mode-alist))

(require 'init-nyan)
(setq-default mode-line-format
	      (list
	       "%e"
	       'mode-line-mule-info
	       'mode-line-modified
	       "  "
	       'mode-line-buffer-identification
	       "  "
	       '(:eval (list (nyan-create)))
	       "%l  "
	       '(:propertize ("" mode-name))
	       '(:eval (get-minor-mode-alist))
	       " %n  "
	       'mode-line-process
	       ))

;; frame size
(when (eq system-type 'windows-nt) ;windows
  (add-to-list 'default-frame-alist '(width . 80))
  (add-to-list 'default-frame-alist '(height . 40)))

(when (eq system-type 'darwin) ;mac os
  (add-to-list 'default-frame-alist '(width . 120))
  (add-to-list 'default-frame-alist '(height . 40)))

;; frame transparent
;(set-frame-parameter (selected-frame) 'alpha (list 85 50))
;(add-to-list 'default-frame-alist (cons 'alpha (list 85 50)))


;; ****************************************
;;     others
;; ****************************************
;; set default backup dir
(setq backup-directory-alist '(("." . "~/.emacs.d/.trash/.backup_emacs"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )
;; set default autosave fir
(setq auto-save-list-file-prefix "~/.emacs.d/.trash/.autosave_emacs")
;; set default authinfo file
(setq auth-sources (list (concat my/ecfg-dir "authinfo")))

;; close emacs start-up
(setq inhibit-startup-message t)
;; close gnus start-up
(setq gnus-inhibit-startup-message t)

;; close bell
(setq ring-bell-function 'ignore)

;; set y->yes, n->no
(fset 'yes-or-no-p 'y-or-n-p)


(provide 'init-frame)
