;;; init-sqlite-mode.el --- sqlite mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sqlite-mode
  :ensure nil
  :defer
  :bind (:map sqlite-mode-map
              ("n" . next-line)
              ("p" . previous-line)))

(provide 'init-sqlite-mode)
;;; init-sqlite-mode.el ends here
