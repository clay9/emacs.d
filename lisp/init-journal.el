;;; init-journal.el --- Journal setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org-journal
  :commands (org-journal-new-entry)
  :mode ("\\.jd$" . org-journal-mode)
  :config
  (setq org-journal-dir "~/my/gtd/journal/"
        org-journal-file-type 'daily
        org-journal-file-format "%Y-%m-%d.jd"
        org-journal-date-format "%Y-%m-%d %A"))

(provide 'init-journal)
;;; init-journal.el ends here
