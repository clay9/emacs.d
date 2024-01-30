;;; init-org-mode.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package org
  :bind ( :map org-mode-map
          ("C-j" . transient/org-mode)
          ;; forward & backwardo
          ("M-p" . org-backward-heading-same-level)
          ("M-n" . org-forward-heading-same-level)
          ("C-k" . org-kill-line)
          ;; forbid
          ("C-'" . nil)
          :map org-src-mode-map
          ("C-c C-c" . org-edit-src-exit))
  :config
  (defun transient/org-mode/org-indent-subtree ()
    "Indent current heading && childs heading"
    (interactive)
    (let ((point (point)))
      (org-with-limited-levels
       (cond ((org-at-heading-p))
	     ((org-before-first-heading-p) (user-error "Not in a subtree"))
	     (t (outline-previous-visible-heading 1) )))
      (let ((begin (org-element-property :begin (org-element-at-point)))
	    (end   (org-element-property :end   (org-element-at-point))))
                                        ;(message "%s, %s, point: %s" begin end point)
        (org-indent-region begin end)
        (goto-char point) )))

  (defun transient/org-mode/org-insert-structure-template (type)
  (interactive
   ;; get key && val
   (list (pcase (org--insert-structure-template-mks)
           ;; if \t, then read input
           (`("\t" . ,_) (read-string "Structure type: "))
           ;; get key && val
           ;; key is tring; val is list
           (`(,key ,val . ,_) (concat key " " val)))))

  (let* ((column (current-indentation))
         ;; get really val
         (key (format "%s" (car (split-string type))))
         (is_src (eql 0 (string-match "s" key)))
         (is_summary_details (eql 0 (string-match "d" key)))
         (val (let* ((ori (substring type (+ 1 (length key))))
                     (real ori))
                (when is_src (setq real (concat "src " ori " -n")))
                (when is_summary_details (setq real "details"))
                real))
         (pos))
    (message "key:%s val:%s" key val)

    ;; get #+begin line
    (if (save-excursion (skip-chars-backward " \t") (bolp))
	(beginning-of-line)
      (insert "\n"))

    (save-excursion
      ;; insert #+begin
      (indent-to column)
      (insert (format "#+begin_%s\n" val))

      ;; line between #+begin and #+end
      (unless (bolp) (insert "\n"))
      (setq pos (point))

      ;; #+end line
      (insert "\n")

      ;; insert #+end
      (indent-to column)
      (insert (format "#+end_%s"  (car (split-string val))))

      ;; insert \n after #+end-line 
      (if (looking-at "[ \t]*$") (replace-match "")
	(insert "\n"))
      (when (and (eobp) (not (bolp))) (insert "\n")))

    ;;
    (goto-char pos)
    (indent-to column)

    ;; insert summary key:+ 保留为不使用    
    (when is_summary_details (org-insert-structure-template "summary"))))
  
  (transient-define-prefix transient/org-mode()
    [[:class transient-column "columns"
	     ("1" "view" org-columns)
	     ("2" "insert" org-columns-insert-dblock)]
     
     [:class transient-column "subtree"
	     ("j" "indent" transient/org-mode/org-indent-subtree)
	     ("n" "o narrow" org-narrow-to-subtree)
	     ("C-n" "c narrow" widen)
             ("r" "refile" org-refile)
	     ("w" "copy" org-copy-subtree)
	     ("y" "paste" org-paste-subtree)
	     ("a" "archive" org-archive-subtree)]

     [:class transient-column "add info"
	     ("SPC" "todo" org-todo)
	     ("RET" "tag" org-set-tags-command)
	     ("p" "property" org-set-property)
	     ("-" "property-" org-priority-down)
	     ("=" "property+" org-priority-up)
	     ("t" "time inactive" org-time-stamp-inactive)
	     ("T" "time active" org-time-stamp)
	     ("e" "effort" org-set-effort)]

     [:class transient-column "link"
	     ("i" "i&e" (lambda() (interactive) (call-interactively 'org-insert-link)))
	     ("o" "open" org-open-at-point)
	     ("b" "goback" org-mark-ring-goto)]

     [:class transient-column "export"
	     ("C-1" "ascill" org-ascii-export-to-ascii)
	     ("C-2" "md" org-md-export-to-markdown)
	     ("C-3" "html" org-html-export-to-html)
	     ("C-4" "texinfo" org-texinfo-export-to-texinfo)]

     [:class transient-column "src"
	     ("s" "add" transient/org-mode/org-insert-structure-template)
	     ("C-j" "exit" org-edit-special)]])

  :config
  
;;; show type
  
  (use-package org-bullets :init (with-eval-after-load 'org (org-bullets-mode)))
  
  ;; when window-system, show image
  ;; (when (window-system)
  ;;   ;; iimage-mode
  ;;   (require 'iimage)
  ;;   (add-hook 'org-mode-hook (lambda () (iimage-mode)))
  ;;   (add-to-list 'iimage-mode-image-regex-alist
  ;;                (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
  ;;       		       "\\)\\]")  1)) )

  (setq
   ;;org-ellipsis "⤵"
   org-hide-emphasis-markers t
   org-pretty-entities t

   org-link-file-path-type 'relative
   org-agenda-inhibit-startup nil
   org-startup-folded t
   org-catch-invisible-edits 'error
   org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  
;;; indent
  
  ;; indent content with headline
  (setq org-adapt-indentation t)
  ;; show same index in org-mode|org-src-mode
  (defun my/org-mode-hook()
    (setq tab-width 4
	  indent-tabs-mode nil) )
  (add-hook 'org-mode-hook 'my/org-mode-hook)
  (add-hook 'org-src-mode-hook 'my/org-mode-hook)



;;; Subscripts and Superscripts ::  a_b not, a_{b} is
  (setq org-use-sub-superscripts '{})
  

;;; Babel languages

  ;; 不提示是否执行代码块
  (setq org-confirm-babel-evaluate nil)
  ;; 执行代码块时, 不新建窗口
  (setq org-src-window-setup 'current-window)

  ;; C-c C-c 执行代码块后, 显示图片
  (defun my/show-image()
    (org-display-inline-images t t))
  (add-hook 'org-babel-after-execute-hook 'my/show-image)

  ;; structure template
  (setq org-structure-template-alist
        '(("e" . "example") ;;int block 不受标记影响
          ("c" . "center") ;;居中显示
          ("d" . "details-summary")
          ("q" . "quote")
          ;; html5
          ("ha" . "aside") ;;侧边显示
          ("hm" . "mark")
          ("hs" . "description")
          ("hbb" . "blocktag")
          ("hbi" . "inlinetag")
          ;; src
          ("sc" . "c++")
          ("ss" . "shell")
	  ("se" . "emacs-lisp")
	  ("sp" . "plantuml :exports results :eval no-export :file xxx.png")))
  
  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (plantuml . t)
     (dot . t)))

  ;; plantuml
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))


;;; ob-plantuml
  
  ;; 使用site-lisp/plantuml-mode 代替 org-mode 绘图.
  ;; plantuml-mode使用 plantuml server绘制, 不需要在本地下载plantuml.jar包
  (defun org-babel-execute:plantuml (body params)
    (let* ((do-export (member "file" (cdr (assq :result-params params))))
	   (out-file (if do-export
		         (or (cdr (assq :file params))
			     (error "No :file provided but :results set to file. For plain text output, set :results to verbatim"))
		       (org-babel-temp-file "plantuml-" ".txt")))

	   (out-file (concat (file-name-directory (buffer-file-name)) out-file))
	   (full-body (org-babel-plantuml-make-body body params)))

      (setq my/temp-out-file out-file)
      ;; TODO 这里这设置了三种输出格式, 实际上要有很多
      (setq plantuml-output-type (pcase (file-name-extension out-file)
				   ("png" "png")
				   ("svg" "svg")
				   ("txt" "txt")))

      ;; hook: write to file
      (add-hook 'plantuml-after-execute-hook
	        #'(lambda()
		    (with-current-buffer plantuml-preview-buffer
		      (write-file my/temp-out-file)
		      (save-buffer)
		      (kill-buffer (current-buffer)))
		    (setq plantuml-after-execute-hook nil)

		    (org-display-inline-images t t)))

      ;; call plantuml execute
      (plantuml-preview-string 0 full-body))))


;; org persist
(with-eval-after-load 'org-persist
  (setq org-persist-directory (concat my/ecfg-dir "org-persist/")))

(provide 'init-org-mode)
;;; init-org-mode.el ends here
