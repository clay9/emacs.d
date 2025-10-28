;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'sub-git-repolist)

(use-package magit
  :commands (magit-status)
  :bind (:map magit-status-mode-map
              ("TAB" . magit-section-cycle))
  :config
  ;;----------------------------------------
  ;;; Magit 基础配置
  ;;----------------------------------------
  (setq-default magit-diff-refine-hunk t)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer (if (and (derived-mode-p 'magit-mode)
                           (memq (with-current-buffer buffer major-mode)
                                 '(magit-process-mode
                                   magit-revision-mode
                                   magit-diff-mode
                                   magit-stash-mode
                                   magit-status-mode)))
                      nil
                    '(display-buffer-same-window)))))

  ;;----------------------------------------
  ;;; Git status buffer 头部和 Section 设置
  ;;----------------------------------------
  (setq magit-status-headers-hook
        '(magit-insert-repo-header
          magit-insert-head-branch-header
          magit-insert-remote-header
          magit-insert-tags-header
          magit-insert-error-header
          magit-insert-diff-filter-header
          magit-insert-push-branch-header))
  (setq magit-module-sections-nested t)
  (setq magit-section-visibility-indicator nil)

  ;; 高亮设置
  (set-face-attribute 'magit-section-highlight nil
                      :extend t
                      :background (face-attribute 'default :background))
  (set-face-attribute 'magit-diff-hunk-heading-highlight nil
                      :extend t
                      :foreground (face-attribute 'default :foreground)
                      :background (face-attribute 'default :background))
  (set-face-attribute 'magit-diff-hunk-heading nil
                      :extend t
                      :foreground (face-attribute 'default :foreground)
                      :background (face-attribute 'default :background))

  ;;----------------------------------------
  ;;; Git diff 设置
  ;;----------------------------------------
  (setq magit-diff-paint-whitespace nil))

(provide 'init-git)
;;; init-git.el ends here
