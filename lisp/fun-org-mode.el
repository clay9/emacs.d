;;; fun-org-mode.el --- Org-mode Functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :config
  ;;----------------------------------------
  ;;; Transient menus
  ;;----------------------------------------
  (transient-define-prefix transient/org-mode ()
    [["Subtree"
      ("n" "!narrow" (lambda()
                       (interactive)
                       (if (buffer-narrowed-p)
                           (widen)
                         (org-narrow-to-subtree))))
      ("r" "refile" org-refile)
      ("w" "copy" org-copy-subtree)
      ("y" "paste" org-paste-subtree)
      ("d" "archive done" org-mode/archive)]
     ["Properties"
      ("t" "todo" org-todo)
      (":" "tag" org-set-tags-command)
      ("p" "property" org-set-property)
      ("e" "effort" org-set-effort)]
     ["TimeStamp"
      ("C-i" "inactive" org-time-stamp-inactive)
      ("C-a" "active" org-time-stamp)]
     ["Links"
      ("i" "insert" org-insert-link)
      ("o" "open" org-open-at-point)
      ("b" "back" org-mark-ring-goto)]
     ["Blocks"
      ("s" "insert template" org-mode/org-insert-structure-template)
      ("C-j" "edit-src" org-edit-special)]
     ["Export" ("C-e" "export" transient/org-export)]
     ["Statistics"
      ("1" "view" org-columns)
      ("2" "insert" org-columns-insert-dblock)]])

  (transient-define-prefix transient/org-export ()
    ["Export"
     ("a" "ascii" org-ascii-export-to-ascii)
     ("m" "markdown" org-md-export-to-markdown)
     ("h" "html" org-html-export-to-html)
     ("t" "texinfo" org-texinfo-export-to-texinfo)])

  (defun org-mode/archive ()
    "Archive current subtree safely:
- mark as done (if it has a todo state),
- close clock if active on this entry,
- archive to task-specific target when buffer name is `task.org`."
    (interactive)
    (let ((todo (org-get-todo-state)))
      ;; ensure it's DONE if it's a todo entry with a state
      (while (and (org-get-todo-state)
                  (not (org-entry-is-done-p)))
        (org-todo))
      ;; close clock if active and matches this entry
      (when (and (org-clock-is-active)
                 (string= (org-entry-get nil "ITEM") org-clock-current-task))
        (org-agenda-clock-out))
      ;; TODONOW 这里要修改, 最好是在文件中设置archive location, 不要写死
      ;; set archive target when in task.org
      (let ((org-archive-location
             (if (string= "task.org" (buffer-name))
                 (if (string= todo "PROJECT")
                     "archive.org::* Project"
                   "archive.org::* Todo && Waiting")
               org-archive-location)))
        (org-archive-subtree))))

  ;;----------------------------------------
  ;;; Utility Functions
  ;;----------------------------------------
  ;; structure template. key 'y'保留, 供函数org-mode/org-insert-structure-template使用
  (defun org-mode/org-insert-structure-template (type)
    (interactive
     (list (pcase (org--insert-structure-template-mks)
             (`("\t" . ,_) (read-string "Structure type: "))
             (`(,key ,val . ,_) (concat key " " val)))))
    (let* ((column (current-indentation))
           (key (format "%s" (car (split-string type)))) ;; get really val
           (before_begin (pcase key
                           ("bd" "#+attr_shortcode: :title ")
                           ("bh" "#+attr_shortcode: info|success|warning|danger")
                           ("y1" "#+attr_shortcode: # ") ;; #在此处为markdown中的标题字体
                           (_ nil)))
           (followed_begin (let* ((shortcode (substring type (+ 1 (length key))))
                                  (is-src (eql 0 (string-match "s" key)))) ;; is babel
                             (if is-src
                                 (concat "src " shortcode)
                               (pcase key
                                 ("hv" "export hugo")
                                 ("hy" "export hugo")
                                 ("bb" "export hugo")
                                 (_ shortcode)))))
           (after_begin (pcase key
                          ("hv" "{{< vimeo >}}")
                          ("hy" "{{< youtube >}}")
                          ("bb" "{{< badge style=\"info|success|warning|danger\" title=\"\" value=\"\" >}}")
                          (_ nil)))
           (insert_another_shortcode (pcase key
                                       ("bt" (list "y1 tab" 3))
                                       (_ nil)))
           (pos))
      (message "key:%s; before_begin:%s; followed_begin:%s; after_begin:%s; insert_another:%s"
               key before_begin followed_begin after_begin insert_another_shortcode)

      ;; get #+begin line
      (if (save-excursion (skip-chars-backward " \t") (bolp))
	  (beginning-of-line)
        (insert "\n"))

      (save-excursion
        ;; insert before_begin
        (when before_begin (indent-to column) (insert before_begin "\n"))

        ;; insert #+begin
        (indent-to column)
        (insert (format "#+begin_%s\n" followed_begin))

        ;; move to #+begin next line
        (unless (bolp) (insert "\n"))
        (indent-to column)
        (setq pos (point))

        ;; insert after_begin
        (when after_begin (insert after_begin) (indent-to column))
        (insert "\n")

        ;; insert #+end
        (indent-to column)
        (insert (format "#+end_%s"  (car (split-string followed_begin))))

        ;; insert \n after #+end-line
        (if (looking-at "[ \t]*$")
            (replace-match "")
	  (insert "\n"))
        (when (and (eobp) (not (bolp))) (insert "\n")))

      ;; goback to pos
      (goto-char pos)
      (indent-to column)

      ;; insert another shortcode
      (when insert_another_shortcode
        (let* ((key_shortcode (car insert_another_shortcode))
               (times (cadr insert_another_shortcode)))
          (dotimes (i times)
            (goto-char pos)
            ;; insert new line
            (when (> (- times i) 1)
              (insert "\n")
              (insert "\n")
              (indent-to column))
            ;; insert shortcode
            (transient/org-mode/org-insert-structure-template key_shortcode)))))))

(provide 'fun-org-mode)
;;; fun-org-mode.el ends here
