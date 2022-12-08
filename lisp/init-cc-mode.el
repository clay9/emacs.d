;; ****************************************************
;; CC Mode
;; ****************************************************
;; .h关联到c++ mode
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)
		("\\.hpp$" . c++-mode)
		("\\.cs$" . c++-mode)
		("\\.proto$" . c++-mode))
	      auto-mode-alist))

;; set myself cpp style
(defconst my-cpp-style
  '((c-basic-offset . 4)
    (c-tab-always-indent . t)
    (c-hanging-braces-alist . ((brace-list-open)
			       (brace-entry-open)
			       (statement-cont)))
    (c-offsets-alist . ((block-open  . 0)
			(block-close . 0)
			(comment-intro . 0)
			(case-label . +)
			(substatement-open . 0))))
  "My C++ Programming Style 2017.01.12")

(c-add-style "MY_CPP_STYLE" my-cpp-style)
(defun my-cpp-mode-common-hook()
  (c-set-style "MY_CPP_STYLE")
  (linum-mode t) ;;增加line num的显示
  (hide-dos-eol) ;;隐藏 "^M"
  (setq tab-width 4
	indent-tabs-mode nil))

(add-hook 'c-mode-common-hook 'my-cpp-mode-common-hook)


;; ****************************************************
;; Function
;; ****************************************************
(defun hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table)) 
  (aset buffer-display-table ?\^M []))


(provide 'init-cc-mode)
