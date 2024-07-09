;;; init-text-hs.el --- sexp  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; by regular
(use-package hideshow
  :diminish hs-minor-mode
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


;;; by treesit
(require 'treesit-fold)
(require 'treesit-fold-indicators)
(global-treesit-fold-mode)

(defun my/ts-shift-tab()
  (interactive)
  (let ((node (treesit-fold--foldable-node-at-pos)))
    (if node
        (my/ts-toggle)
      (my/ts-toggle-all))))

(defun my/ts-hide()
  (interactive)
  (let ((node (treesit-fold--foldable-node-at-pos))
        (pos (point))
        (act nil))
    (dolist (son (treesit-node-children node))
      (dolist (grandson (treesit-node-children son))
        (let* ((ov (treesit-fold-overlay-at grandson))
              (range (treesit-fold--get-fold-range grandson))
              (ov2 (treesit-fold--create-overlay range)))
          (when (not (equal ov ov2))
            (run-hooks 'treesit-fold-on-fold-hook)
            (setq act t)))))
    (goto-char pos)
    (unless act (treesit-fold-close node))))

(defun my/ts-toggle()
  (interactive)
  (let* ((node (treesit-fold--foldable-node-at-pos))
         (ov (treesit-fold-overlay-at node)))
    (if ov
        (treesit-fold-open-recursively)
      (my/ts-hide))))

(defvar my/ts-hide-all nil)
(defun my/ts-toggle-all()
  (interactive)
  (setq my/ts-hide-all (not my/ts-hide-all))
  (if my/ts-hide-all (treesit-fold-close-all)
    (treesit-fold-open-all)))

;;; hook
;; if treesit-fold available, use treesit-fold. otherwise, hs minor
(add-hook 'prog-mode-hook
          (lambda()
            (unless (treesit-parser-list)
              (hs-minor-mode))))


;;; key
(defun my/shift-tab()
  (interactive)
  (cond ((bound-and-true-p treesit-fold-mode)
         (my/ts-shift-tab))
        ((bound-and-true-p hs-minor-mode)
         (my/hs-shift-tab))))

(provide 'init-text-hs)
;;; init-text-hs.el ends here
