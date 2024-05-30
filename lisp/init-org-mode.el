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
  (defun transient/org-mode/archive ()
    (interactive)
    (let* ((todo-state (org-get-todo-state)))
      ;; 1. check todo-state
      (while (not (org-entry-is-done-p))
        (org-todo))
      ;; 2. close clock if in this item
      (when (and (org-clock-is-active)
                 (string= (org-entry-get nil "ITEM") org-clock-current-task))
        (org-agenda-clock-out))
      ;; 3. move to %project%_archive or archive.org::
      (if (string= "task.org" (buffer-name (current-buffer)))
          (let* ((org-archive-location (if (string= todo-state "PROJECT")
                                           "archive.org::* Project"
                                         "archive.org::* Todo && Waiting")))
            (org-archive-subtree))
        (org-archive-subtree))))
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
           ;; key is string; val is list
           (`(,key ,val . ,_) (concat key " " val)))))

  (let* ((column (current-indentation))
         ;; get really val
         (key (format "%s" (car (split-string type))))
         (before_begin (cond ((eql 0 (string-match "he" key)) ;expand
                              "#+attr_shortcode: expand-name \"...\"")
                             ((eql 0 (string-match "hh" key)) ;hint 
                              "#+attr_shortcode: info | warning | danger")
                             (t nil)))
         (followed_begin (let* ((ori (substring type (+ 1 (length key)))))
                          (cond ((eql 0 (string-match "sp" key)) ;src plantuml
                                 (concat "src " ori " -n"))
                                ((eql 0 (string-match "s" key)) ;src
                                 (concat "src " ori " -n"))
                                (t ori))))
         (after_begin (cond ((eql 0 (string-match "hc" key)) ;columns
                             "@@hugo:<--->@@")
                             (t nil)))
         (pos))
    (message "key:%s; before_begin:%s; followed_begin:%s" key before_begin followed_begin)

    ;; get #+begin line
    (if (save-excursion (skip-chars-backward " \t") (bolp))
	(beginning-of-line)
      (insert "\n"))

    (save-excursion
      ;; insert before_begin
      (when before_begin
        (indent-to column)
        (insert before_begin)
        (insert "\n"))
      
      ;; insert #+begin
      (indent-to column)
      (insert (format "#+begin_%s\n" followed_begin))

      ;; line between #+begin and #+end
      (unless (bolp) (insert "\n"))
      (setq pos (point))

      ;; insert after_begin
      (when after_begin
        (indent-to column)
        (insert "\n")
        (indent-to column)
        (insert after_begin)
        (indent-to column))

      ;; #+end line
      (insert "\n")

      ;; insert #+end
      (indent-to column)
      (insert (format "#+end_%s"  (car (split-string followed_begin))))

      ;; insert \n after #+end-line
      (if (looking-at "[ \t]*$") (replace-match "")
	(insert "\n"))
      (when (and (eobp) (not (bolp))) (insert "\n")))

    ;;
    (goto-char pos)
    (indent-to column)))
  (transient-define-prefix transient/org-statistics()
    ["columns"
     ("1" "view" org-columns)
     ("2" "insert" org-columns-insert-dblock)])
  (transient-define-prefix transient/org-timestamp()
    ["timestamp"
     ("t" "time inactive" org-time-stamp-inactive)
     ("T" "time active" org-time-stamp)])
  (transient-define-prefix transient/org-export()
    ["export"
     ("a" "ascill" org-ascii-export-to-ascii)
     ("m" "md" org-md-export-to-markdown)
     ("h" "html" org-html-export-to-html)
     ("t" "texinfo" org-texinfo-export-to-texinfo)])

  (transient-define-prefix transient/org-mode()
    [["subtree"
      ("j" "indent" transient/org-mode/org-indent-subtree)
      ("n" "o narrow" org-narrow-to-subtree)
      ("C-n" "c narrow" widen)
      ("r" "refile" org-refile)
      ("w" "copy" org-copy-subtree)
      ("y" "paste" org-paste-subtree)
      ("a" "archive" org-archive-subtree)]

     ["statistics"
      ("l" "statistics" transient/org-statistics :transient t)]

     ["add info"
      ("t" "todo" org-todo)
      (":" "tag" org-set-tags-command)
      ("-" "property-" org-priority-down)
      ("=" "property+" org-priority-up)
      ("p" "property" org-set-property)
      ("e" "effort" org-set-effort)
      ("d" "archive done" transient/org-mode/archive)]

     ["timestamp"
      ("s" "timestamp" transient/org-timestamp)]

     ["link"
      ("i" "i&e" (lambda() (interactive) (call-interactively 'org-insert-link)))
      ("o" "open" org-open-at-point)
      ("b" "goback" org-mark-ring-goto)]

     ["src"
      ("s" "add" transient/org-mode/org-insert-structure-template)
      ("C-j" "exit" org-edit-special)]

     ["export"
      ("C-e" "export" transient/org-export)]])

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
        '(;; org normal
          ("e" . "example")
          ("c" . "center")
          ("q" . "quote")
          ;; html5
          ;;("ha" . "aside") ;; not work. need css
          ("hc" . "columns")
          ("he" . "expand")
          ("hh" . "hint")          
          ;; src
          ("sa" . "artist")
          ("sc" . "C++")
          ("se" . "emacs-lisp")
          ("ss" . "shell")
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
