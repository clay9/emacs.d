;;; export-org-to-md.el ---   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ox-hugo)

;;; org-mode
(require 'org)
;; when export, do not raise an error on broken links. not work to publish
(setq org-export-with-broken-links t)
;; set org-babel exports only code
(setq org-babel-default-header-args
      '((:session . "none")
	(:results . "replace")
	(:exports . "code")
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
;; Subscripts and Superscripts ::  a_b not, a_{b} is
(setq org-export-with-sub-superscripts '{})
;; support html5
;;(setq org-html-html5-fancy nil)
;; rm blank-lines
;;(setq org-ascii-headline-spacing nil)


(defun my/hugo-get-weight (name)
  "Get numeric weight from directory or file NAME, default to 0."
  (if (string-match "\\`\\([0-9]+\\)_" name)
      (match-string 1 name)
    "0"))

(defun my/hugo-generate-root-index-header (file root-dir)
  "Generate Hugo Org header for FILE relative to ROOT-DIR."
  (format
   "#+date: %s\n#+hugo_section: /\n#+hugo_bundle: .\n#+export_file_name: _index\n#+hugo_weight: 1\n#+hugo_draft: false\n#+hugo_auto_set_lastmod: t\n#+hugo_paired_shortcodes: qr %%columns %%details %%hint mermaid %%steps tabs tab\n\n"
   (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun my/hugo-generate-header (file root-dir)
  "Generate Hugo Org header for FILE relative to ROOT-DIR."
  (let* ((file-name (file-name-nondirectory file))
         (base-name (file-name-sans-extension file-name))
         (parent-dir (file-name-directory file))
         (org-dir (expand-file-name "org" root-dir))
         (relative-dir (directory-file-name (file-relative-name parent-dir org-dir)))
         (is-index (string= base-name "_index"))
         (bundle (if is-index relative-dir (concat relative-dir "/" base-name)))
         (weight (my/hugo-get-weight (if is-index (file-name-nondirectory (directory-file-name parent-dir)) base-name)))
         (export-name (if is-index "_index" "index"))
         (has-collapse (with-temp-buffer
                         (insert-file-contents file)
                         (re-search-forward "bookCollapseSection" nil t)))
         (collapse (if is-index "true" "false")))
    (format
     "#+date: %s\n#+hugo_section: docs\n#+hugo_bundle: %s\n#+export_file_name: %s\n#+hugo_weight: %s\n#+hugo_draft: false\n#+hugo_auto_set_lastmod: t\n%s#+hugo_paired_shortcodes: qr %%columns %%details %%hint mermaid %%steps tabs tab\n\n"
     (format-time-string "%Y-%m-%d %H:%M:%S")
     bundle
     export-name
     weight
     (if has-collapse
         ""
       (format "#+hugo_custom_front_matter: :bookCollapseSection %s\n" collapse)))))

(defun my/hugo-export-file (file root-dir)
  "Export FILE to Markdown."
  (with-current-buffer (find-file-noselect file)
    (let* ((org-export-use-babel nil)
           (is-root-index (string= (expand-file-name file)
                                   (expand-file-name "org/_index.org" root-dir)))
           (header (if is-root-index
                       (my/hugo-generate-root-index-header file root-dir)
                     (my/hugo-generate-header file root-dir))))
      (goto-char (point-min))
      (message "%s" header)
      (insert header)
      (save-buffer)
      (org-hugo-export-to-md))))

(defun my/hugo-export-all (root-dir)
  "Export all Org files under ROOT-DIR."
  (interactive "DProject root directory: ")
  (setq org-hugo-base-dir root-dir)
  (dolist (file (directory-files-recursively
                 (expand-file-name "org" root-dir)
                 "\\.org\\'"))
    (message "\n\n=====Exporting: %s==========" file)
    (my/hugo-export-file file root-dir)))

(provide 'export-org-to-md)
;;; export-org-to-md.el ends here
