;;; init-project.el ---   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'project
  ;; switch project without menu
  (setq project-list-file (concat my/ecfg-dir "projects"))
  (setq project-switch-commands 'project-find-file))


(provide 'init-project)
;;; init-project.el ends here
