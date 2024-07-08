;;; init-text-hs.el --- sexp  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :diminish hs-minor-mode
  :bind ( :map hs-minor-mode-map
          ("<backtab>" . my/hs-shift-tab))
  :config
  (defun my/hs-shift-tab ()
    "
1. out block, toggle all.
2. in block, 隐藏sub, 没有则隐藏自己; 已经隐藏则显示自己和sub"
    (interactive)
    (cond ((hs-inside-comment-p) (hs-toggle-hiding))
          ((my/hs-inside-block-p) (my/hs-toggle-block))
          (t (my/hs-toggle-all))))

  (defvar my/hs-hide-all nil)
  (defun my/hs-toggle-all ()
    "toggle hide | show all"
    (setq my/hs-hide-all (not my/hs-hide-all))
    (if my/hs-hide-all (hs-hide-all)
      (hs-show-all)))

  (defun my/hs-toggle-block ()
    "toggle hide|show [block, level]"
    (if (hs-already-hidden-p) (hs-show-block)
      (if (my/hs-is-have-level) (hs-hide-level 1)
        (hs-hide-block))))

  (defun my/hs-is-have-level ()
    "check is have unhidden level"
    (setq here (point))
    (setq have-level nil)
    (when (hs-find-block-beginning)
      (setq minp (1+ (point)))
      (funcall hs-forward-sexp-func 1)
      (setq maxp (1- (point))))
    (goto-char minp)
    (while (progn
	     (forward-comment (buffer-size))
	     (and (< (point) maxp)
		  (re-search-forward hs-block-start-regexp maxp t)))
      (when (save-match-data
	      (not (nth 8 (syntax-ppss)))) ; not inside comments or strings
        ;; 如果block-start与block-end在同一行, 也应该认为是已经hidden了
        (setq pmin (point))
        (when (hs-find-block-beginning)
	  (funcall hs-forward-sexp-func 1)
	  (setq pmax (1- (point))))
        (goto-char pmin)
        (unless (or (hs-already-hidden-p)
	        (<= (count-lines pmin pmax) 1))
	  (setq have-level t))))
    (goto-char here)
    (if have-level t nil))

  (defun my/hs-inside-block-p ()
    "check is in block"
    (save-excursion
      (or (hs-looking-at-block-start-p)
	  (hs-find-block-beginning)
          (hs-inside-comment-p)))))


(provide 'init-text-hs)
;;; init-text-hs.el ends here
