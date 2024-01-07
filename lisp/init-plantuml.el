;;; init-plantuml.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package plantuml-mode
  :load-path "site-lisp/plantuml-mode"
  :mode ("\\.puml\\'" . plantuml-mode)
  :interpreter ("plantuml" . plantuml-mode)
  :bind ( :map plantuml-mode-map
          ("C-j" . transient/plantuml-mode))
  :config
  (transient-define-prefix transient/plantuml-mode()
    [[:class transient-column "action"
	     ("j" "preview" plantuml-preview)
	     ("s" "save image"
              (lambda() (interactive)
		(with-current-buffer plantuml-preview-buffer
		  (save-buffer))))]])
  :config
  (setq plantuml-default-exec-mode 'server
        plantuml-output-type "png"
        plantuml-indent-level 4
        tab-width 4
        indent-tabs-mode nil))

(provide 'init-plantuml)
;;; init-plantuml.el ends here
