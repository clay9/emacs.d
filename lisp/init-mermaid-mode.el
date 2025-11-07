;;; init-mermaid-mode.el --- Mermaid diagram editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mermaid-mode
  :config
  ;;----------------------------------------
  ;;; Mermaid Config
  ;;----------------------------------------
  ;; Mermaid CLI in Docker
  (setq mermaid-mmdc-location "docker")
  (setq mermaid-flags "run --rm -u 1000 -v /tmp:/tmp ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:latest")

  ;;----------------------------------------
  ;;; Mermaid in Org Babel
  ;;----------------------------------------
  (use-package ob-mermaid
    :after org
    :config
    ;; mmdc 是 @mermaid-js/mermaid-cli 提供的命令
    (setq ob-mermaid-cli-path
          (concat "docker run --rm "
                  "-u 1000 "
                  "-v /var:/var "
                  "-v /Users:/Users "
                  "ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:latest"))

    ;; 在 org-babel 中启用 mermaid
    (add-to-list 'org-babel-load-languages '(mermaid . t))
    ;; ensure major-mode mapping
    (add-to-list 'org-src-lang-modes '("mermaid"  . mermaid))))

(provide 'init-mermaid-mode)
;;; init-mermaid-mode.el ends here
