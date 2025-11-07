;;; init-project.el ---   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'project
  (setq project-list-file (concat my/ecfg-dir "projects"))
  ;; Call 'project-switch-project' without promt menu
  (setq project-switch-commands 'project-find-file))

(provide 'init-project)
;;; init-project.el ends here
