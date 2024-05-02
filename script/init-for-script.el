;;; init-for-script.el ---   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "script" user-emacs-directory))


;;; TODO when complete github-action for emacs, then del this

;; (require 'package)
;; (defun require-package (package &optional min-version no-refresh)
;;   "Install given PACKAGE, optionally requiring MIN-VERSION.
;; If NO-REFRESH is non-nil, the available package lists will not be
;; re-downloaded in order to locate PACKAGE."
;;   (when (stringp min-version)
;;     (setq min-version (version-to-list min-version)))
;;   (or (package-installed-p package min-version)
;;       (let* ((known (cdr (assoc package package-archive-contents)))
;;              (best (car (sort known (lambda (a b)
;;                                       (version-list-<= (package-desc-version b)
;;                                                        (package-desc-version a)))))))
;;         (if (and best (version-list-<= min-version (package-desc-version best)))
;;             (package-install best)
;;           (if no-refresh
;;               (error "No version of %s >= %S is available" package min-version)
;;             (package-refresh-contents)
;;             (require-package package min-version t)))
;;         (package-installed-p package min-version))))


(require 'init-package)
(require 'export-org-to-md)

;; org-files 输出为 md-files
;; github action中, 该fun 会在./hugo中执行. 因此这里传递.即可
(my/hugo-export-all (expand-file-name "."))

;;; init-for-script.el ends here
