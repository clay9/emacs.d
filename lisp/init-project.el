;;; init-project.el ---   -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package project
  :ensure nil
  :config
  ;; Call 'project-switch-project' without promt menu
  (setq project-switch-commands '(project-find-file)))

(provide 'init-project)
;;; init-project.el ends here
