(require-package 'org-bullets)
(require 'org-bullets)
(require 'iimage)
(require 'org)

;; ****************************************************
;; 外在显示
;; ****************************************************
;; 在非window-system中(详见C-h v window-system), 不显示图标和图片
(when (window-system)
  ;; 使用图标代替heading中的*
  (add-hook 'org-mode-hook
	    (lambda ()
	      (org-bullets-mode t)
	      (iimage-mode)
	      ))
  ;; ⤵ 替换 省略号
  (setq org-ellipsis "⤵")
  ;; 显示图片
  (add-to-list 'iimage-mode-image-regex-alist
	       (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
			     "\\)\\]")  1)) )

;; 默认显示为 overview
(setq
 org-startup-folded t
 org-agenda-inhibit-startup nil)

;; ****************************************************
;; others
;; ****************************************************
;; 文本默认与标题对齐
(setq org-adapt-indentation t)
;; 编辑折叠的内容时候 提示错误
(setq org-catch-invisible-edits 'error)

;; hook
(defun my-org-mode-hook()
  (setq tab-width 4
	indent-tabs-mode nil) )
(add-hook 'org-mode-hook 'my-org-mode-hook)
(add-hook 'org-src-mode-hook 'my-org-mode-hook); 可以防止artist-mode与src中显示的效果不同


;; ****************************************************
;; Babel languages
;; ****************************************************
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (emacs-lisp . t)
   (plantuml . t)
   (dot . t)))

;; TODO 要么删除, 要么修改正确路径
(setq org-plantuml-jar-path
      (expand-file-name "~/emacs/plantuml.jar"))


(provide 'init-org-mode)
