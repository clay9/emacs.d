;;; init-completion.el --- completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;;; completion 通用配置
(setq tab-always-indent 'complete)


;;; complete -- base on emacs completion, provide candidates
(use-package consult
  :config
  ;; buffer filter
  (add-to-list 'consult-buffer-filter "\\`\\*Ibuffer\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Calendar\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Disabled Command\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Backtrace\\*\\'")
  (add-to-list 'consult-buffer-filter "\\*EGLOT*")
  (add-to-list 'consult-buffer-filter "magit-process:*")
  
  :init
  (defun consult/gtd-buffers()
    (let* ((buf-list (mapcar 'get-file-buffer (directory-files-recursively "~/my/gtd" ".org")))
           (result))
      (dolist (buf buf-list)
        (when buf
          (add-to-list 'result (buffer-name buf) t)))
      result))
  (defun consult/nerver-show-buffers ()
    '("repos my" "repos company" "repos qy" ".DS_Store" "*Messages*" "*Org Agenda*" "*Magit Repositories*"))
      
  (defvar consult--source-buffer
    `( :name     "Buffer"
       :narrow   ?b
       :category buffer
       :face     consult-buffer
       :history  buffer-name-history    
       :state    ,#'consult--buffer-state
       :default  t
       :items
       ,(lambda () (consult--buffer-query :sort 'visibility
                                          :predicate
                                          (lambda(buf)
                                            (not (or (member (buffer-name buf) (consult/gtd-buffers))
                                                     (member (buffer-name buf) (consult/nerver-show-buffers)))))
                                          :as #'buffer-name)))
    "Buffer candidate source for `consult-buffer'.")
  (defvar consult--source-hidden-buffer
    `( :name     "Hidden Buffer"
       :narrow   ?\s
       :hidden   t
       :category buffer
       :face     consult-buffer
       :history  buffer-name-history
       :action   ,#'consult--buffer-action
       :items
       ,(lambda () (consult--buffer-query :sort 'visibility
                                          :filter 'invert
                                          :as #'buffer-name)))
    "Hidden buffer candidate source for `consult-buffer'.")
  (defvar consult--source-recent-file
    `( :name     "File"
       :narrow   ?f
       :category file
       :hidden   t
       :preview-key nil
       :face     consult-file
       :history  file-name-history
       :state    ,#'consult--file-state
       :new      ,#'consult--file-action
       :enabled  ,(lambda () recentf-mode)
       :items
       ,(lambda ()
          (let ((ht (consult--buffer-file-hash))
                items)
            (dolist (file (bound-and-true-p recentf-list) (nreverse items))
              ;; Emacs 29 abbreviates file paths by default, see
              ;; `recentf-filename-handlers'.  I recommend to set
              ;; `recentf-filename-handlers' to nil to avoid any slow down.
              (unless (eq (aref file 0) ?/)
                (let (file-name-handler-alist) ;; No Tramp slowdown please.
                  (setq file (expand-file-name file))))
              (unless (gethash file ht)
                (push (consult--fast-abbreviate-file-name file) items))))))
    "Recent file candidate source for `consult-buffer'.")
  (defvar consult--source-bookmark
    `( :name     "Bookmark"
       :narrow   ?m
       :category bookmark
       :hidden   t
       :preview-key nil
       :face     consult-bookmark
       :history  bookmark-history
       :items    ,#'bookmark-all-names
       :state    ,#'consult--bookmark-state)
    "Bookmark candidate source for `consult-buffer'.")
  (defvar consult--source-gtd-source
    `( :name     "GTD Buffer"
       :narrow   ?g
       :category buffer
       :hidden   t
       :face     consult-buffer
       :history  buffer-name-history
       :state    ,#'consult--buffer-state
       :items    ,#'consult/gtd-buffers))
  
  :config
  (add-to-list 'consult-buffer-sources 'consult--source-gtd-source 'append))


;;; add ex-info for candidates
(use-package marginalia
  :hook (after-init . marginalia-mode))


;;; filter candidates
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;; minibuffer complete-ui
(use-package vertico
  :hook (after-init . vertico-mode))


;;; buffer complete-ui (instead of company-mode)
(use-package corfu
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))
(use-package corfu-terminal
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))


;;; actions for minibuffer && buffer
(use-package embark)

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

(provide 'init-completion)
;;; init-completion.el ends here
