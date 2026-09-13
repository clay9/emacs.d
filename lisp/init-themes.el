;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Load and configure default themes for Emacs

(use-package dracula-theme
  :config
  (condition-case nil
      (load-theme 'dracula t)
    (error (load-theme 'wombat t))))

;;; Customize faces
(with-eval-after-load 'hi-lock
  (set-face-attribute 'hi-yellow nil :background "color-29"))


;;;; Magit Diff
(with-eval-after-load 'magit-section
  (set-face-attribute 'magit-section-highlight nil
                      :extend t
                      :background (face-attribute 'default :background)))

(with-eval-after-load 'magit-diff
  (set-face-attribute 'magit-diff-removed-highlight nil :background nil)
  (set-face-attribute 'magit-diff-added-highlight nil :background nil)
  (set-face-attribute 'magit-diff-hunk-heading-highlight nil
                      :extend t
                      :foreground (face-attribute 'default :foreground)
                      :background (face-attribute 'default :background))
  (set-face-attribute 'magit-diff-hunk-heading nil
                      :extend t
                      :foreground (face-attribute 'default :foreground)
                      :background (face-attribute 'default :background)))

(with-eval-after-load 'diff-mode
  (set-face-attribute 'diff-refine-removed nil :foreground "#ff5555" :background nil))


;;;; org mode
(with-eval-after-load 'org-faces
  (set-face-attribute 'org-headline-done nil :strike-through nil)  ;; 去除DONE heading中的删除线
  (set-face-attribute 'org-drawer nil :foreground "gray60" :height 0.9)
  (set-face-attribute 'org-special-keyword nil :foreground "gray60" :height 0.9)
  (set-face-attribute 'org-property-value nil :foreground "gray60" :height 0.9)
  ;; org clock
  (set-face-attribute 'org-agenda-clocking nil :extend t :background "color-29")
  ;; today face
  (set-face-attribute 'org-agenda-date-today nil
                      :weight 'bold
                      :italic nil
                      :underline '(:color foreground-color :style line)
                      :inherit '(org-agenda-date))
  (set-face-attribute 'org-column nil
                      :background (face-attribute 'default :background)
                      :height     (face-attribute 'default :height)
                      :family     (face-attribute 'default :family)))


;;;; 调整终端下的Corfu face
(with-eval-after-load 'corfu
  (set-face-attribute 'corfu-default nil
                      :foreground "color-240"))


(provide 'init-themes)
;;; init-themes.el ends here
