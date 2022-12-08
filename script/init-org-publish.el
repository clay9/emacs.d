;; org-info.js 输出为web样式(org-info样式)
(require-package 'htmlize)
(require 'ox-publish)
(require 'ox-html)
(require 'ox)

;; when export, do not raise an error on broken links -- 对publish无效
(setq org-export-with-broken-links t)
;; remove validate link
(setq org-html-validation-link nil)
;; alway publish all
(setq org-publish-use-timestamps-flag nil)

;; publish
(setq org-publish-project-alist
      '(("root"
	 :base-directory "./blog_release"
	 :publishing-directory "./content/org-info"
	 :publishing-function org-html-publish-to-html
	 :with-toc t
	 :with-broken-links t  ;没有效果
	 :html-link-home "/"
	 :html-use-infojs t)
	("script"
	 :base-directory "./blog_release/script"
	 :base-extension "js"
	 :publishing-directory "./content/org-info/script"
	 :publishing-function org-publish-attachment)
	("emacs_IDE"
	 :base-directory "./blog_release/emacs_IDE"
	 :publishing-directory "./content/org-info/emacs_IDE"
	 :publishing-function org-html-publish-to-html
	 :with-toc t
	 :html-link-home "/"
	 :html-link-up "../emacs.html"
	 :html-use-infojs t)
	("emacs_com"
	 :base-directory "./blog_release/emacs_com"
	 :publishing-directory "./content/org-info/emacs_com"
	 :publishing-function org-html-publish-to-html
	 :with-toc t
	 :html-link-home "/"
	 :html-link-up "../emacs.html"
	 :html-use-infojs t)))


;; ********************
;; main
;; ********************
;; 调用之前 请进入~/hugo目录
(org-publish-all t nil)
