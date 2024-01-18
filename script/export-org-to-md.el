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
;;(setq org-list-two-spaces-after-bullet-regexp nil)


;;; ox-hugo
;; use org-mode-generate-toc
(setq org-hugo-export-with-to t)


;; fun: ox-hugo (所有org文件) ==> md文件
(defun my/hugo-export-all (root_dir)
  (interactive)
  (setq org-hugo-base-dir root_dir)
  (dolist (file (directory-files-recursively root_dir ".org"))
    (with-current-buffer (find-file-noselect file)
      (org-hugo-export-to-md)))
  (my/hugo-rm-br root_dir))

;; fun: rm <br/> after plain-list
(defun my/hugo-rm-br(root_dir)
  (dolist (file (directory-files-recursively root_dir ".md"))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (save-restriction
          (beginning-of-buffer)
          (while (not (eobp))
            (let* ((info (thing-at-point 'line))
                   (m1 (string-match "^[ \t]*\\(\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)\\(?:[ \t]*+\\|$\\)\\)" info))
                   (m2 (string-match "<br/>[ \t]*?$" info)))
              (when (and m1 m2)
                (beginning-of-line)
                (delete-region (point) (line-end-position))
                (insert (substring info 0 m2))))
            (forward-line 1)))))))

;; org-files 输出为 md-files
;; github action中, 该fun 会在./hugo中执行. 因此这里传递.即可
(my/hugo-export-all (expand-file-name "."))

(provide 'export-org-to-md)
;;; export-org-to-md.el ends here
