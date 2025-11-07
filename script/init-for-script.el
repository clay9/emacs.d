;;; init-for-script.el ---   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "script" user-emacs-directory))

(require 'init-package)
(require 'export-org-to-md)

;; org-files 输出为 md-files
;; github action中, 该fun 会在./hugo中执行. 因此这里传递.即可
(my/hugo-export-all (expand-file-name "."))

;;; init-for-script.el ends here
