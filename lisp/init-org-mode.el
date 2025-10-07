;;; init-org-mode.el --- Org-mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'fun-org-mode)

(use-package org
  :bind ( :map org-mode-map
          ("C-j" . transient/org-mode)
          ("M-p" . org-backward-heading-same-level)
          ("M-n" . org-forward-heading-same-level)
          ("C-k" . org-kill-line)
          ("C-'" . nil)
          :map org-src-mode-map
          ("C-c C-c" . org-edit-src-exit))
  :config
  ;;----------------------------------------
  ;;; Visual / UI tweaks
  ;;----------------------------------------
  (use-package org-modern
    :hook (org-mode . global-org-modern-mode))

  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'error
   org-insert-heading-respect-content t
   org-adapt-indentation t
   ;; Subscripts and Superscripts ::  a_b not, a_{b} is
   org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
   org-use-sub-superscripts '{}

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-startup-folded 'fold)

  ;; 每次保存Org文件前自动缩进heading
  (add-hook 'before-save-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (save-excursion
                (goto-char (point-min))
                (while (outline-next-heading)
                  (org-indent-region (point) (org-end-of-subtree t)))))))

  ;;----------------------------------------
  ;;; Structure Templates
  ;;----------------------------------------
  (setq org-structure-template-alist
        '(("c"  . "center")
          ("e"  . "example")
          ("q"  . "quote")
          ;; src templates start with 's' key in our usage above
          ("sa" . "artist")
          ("sc" . "C++ :hl_lines")
          ("se" . "emacs-lisp")
          ("ss" . "shell")
          ("sp" . "plantuml :exports results :eval no-export :file xxx.png")
          ;; Hugo shortcodes
          ("hq" . "qr")
          ("hv" . "vimeo")
          ("hy" . "youtube")
          ;; Hugo Book shortcodes
          ("bb" . "badge")
          ("bc" . "columns")
          ("bd" . "details")
          ("bh" . "hint")
          ("bm" . "mermaid")
          ("bs" . "steps")
          ("bt" . "tabs")))

  ;;----------------------------------------
  ;;; Babel: execution behavior
  ;;----------------------------------------
  (setq
   ;; 不提示是否执行代码块
   org-confirm-babel-evaluate nil
   ;; 执行代码块时, 不新建窗口
   org-src-window-setup 'current-window)

  ;; 执行代码块后, 刷新图片
  (add-hook 'org-babel-after-execute-hook
            (lambda()
              (org-display-inline-images t t)))

  ;;----------------------------------------
  ;;; Misc: persistence, ctags fix, performance
  ;;----------------------------------------
  (with-eval-after-load 'org-persist
    (setq org-persist-directory (expand-file-name "org-persist/" my/ecfg-dir)))

  ;; fix org-ctags bug: https://emacs.stackexchange.com/questions/76351/how-to-follow-an-internal-link-in-recent-org-mode
  (with-eval-after-load 'org-ctags
    (setq org-open-link-functions nil)))

(provide 'init-org-mode)
;;; init-org-mode.el ends here
