;;; export-org-to-md.el ---   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ox-hugo)

;;; org-mode
(require 'org)
;; when export, do not raise an error on broken links. not work to publish
(setq org-export-with-broken-links t)
;; set org-babel exports both
(setq org-babel-default-header-args
      '((:session . "none")
	(:results . "replace")
	(:exports . "both")
	(:cache . "no")
	(:noweb . "no")
	(:hlines . "no")
	(:tangle . "no")))
;; no author
;;(setq org-export-with-author nil)
;; no confirm eval
(setq org-confirm-babel-evaluate nil)
;; line breaks when exporting
(setq org-export-preserve-breaks t)
;; support html5
(setq org-html-html5-fancy t)
;; Subscripts and Superscripts ::  a_b not, a_{b} is
(setq org-export-with-sub-superscripts '{})
;; rm blank-lines
(setq org-ascii-headline-spacing nil)
;; rm blank-lines -- TODO test
(setq org-list-two-spaces-after-bullet-regexp nil)


;;; ox-hugo
;; use org-mode-generate-toc
(setq org-hugo-export-with-to t)


;; fun: ox-hugo (所有org文件) ==> md文件
(defun my/hugo-export-all (root_dir)
  (interactive)
  (setq org-hugo-base-dir root_dir)
  (dolist (file (directory-files-recursively root_dir ".org"))
    (with-current-buffer (find-file-noselect file)
      (org-hugo-export-to-md))))
          
(provide 'export-org-to-md)
;;; export-org-to-md.el ends here
