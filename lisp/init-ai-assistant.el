;;; init-ai-assistant.el --- AI assistant  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; codeium
(use-package codeium
  :vc (:url "https://github.com/Exafunction/codeium.el.git" :rev :newest)
  :commands (codeium-init)
  :config
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))

(provide 'init-ai-assistant)
;;; init-ai-assistant.el ends here
