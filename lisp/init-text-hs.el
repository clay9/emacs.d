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
(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold.git" :rev :newest)
  :config
  (require 'treesit-fold-indicators)
  (global-treesit-fold-mode))

(defun my/ts-shift-tab()
  (let* ((in-multi-comment (my/ts-in-multi-comment))
        (node2 (treesit-fold--foldable-node-at-pos)))
    (if in-multi-comment
        (my/ts-toggle-comment)
      (if node2
          (my/ts-toggle)
        (my/ts-toggle-all)))))

(defun my/ts-hide()
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
  (let* ((node (treesit-fold--foldable-node-at-pos))
         (ov (treesit-fold-overlay-at node)))
    (if ov
        (treesit-fold-open-recursively)
      (my/ts-hide))))

(defvar my/ts-hide-all nil)
(defun my/ts-toggle-all()
  (setq my/ts-hide-all (not my/ts-hide-all))
  (if my/ts-hide-all (treesit-fold-close-all)
    (treesit-fold-open-all)))

(defun my/ts-node-range-continue(node)
  (let* ((get-node (lambda(node next)
                     (let* ((cnode node) result)
                       (while (and cnode
                                   (equal (treesit-node-type node)
                                          (treesit-node-type cnode)))
                         (setq result cnode)
                         (setq cnode (if next (treesit-node-next-sibling cnode)
                                       (treesit-node-prev-sibling cnode))))
                       result)))
         (first-node (funcall get-node node nil))
         (last-node (funcall get-node node t))
         (beg (treesit-node-start first-node))
         (end (treesit-node-end last-node))
         (range (cons beg end)))
    range))
(defun my/ts-toggle-comment()
  (treesit-fold-line-comment-mode)
  (let* ((node (treesit-node-at (point)))
         (prefix "//")
         (prefix-len (length prefix))
         (node-range (my/ts-node-range-continue node))
         (range (cons (+ (car node-range) prefix-len) (cdr node-range)))
         (ov (thread-last (overlays-in (car range) (cdr range))
                          (seq-filter (lambda (ov)
                                        (and (eq (overlay-get ov 'invisible) 'treesit-fold)
                                             (= (overlay-start ov) (car range))
                                             (= (overlay-end ov) (cdr range)))))
                          car))
         (ov2 (treesit-fold--create-overlay range)))
    (when (equal ov ov2)
      (let* ((nodes (treesit-fold--overlays-in 'invisible 'treesit-fold (car range) (cdr range))))
        (mapc #'delete-overlay nodes)))
    (run-hooks 'treesit-fold-on-fold-hook)))
(defun my/ts-in-multi-comment()
  (let* ((node (treesit-node-at (point)))
        (result nil))
    (when (equal (treesit-node-type node) "comment")
      (let* ((range-node (my/ts-node-range-continue node))
             (node-start (treesit-node-start node))
             (node-end (treesit-node-end node)))
        (when (or (not (equal node-start (car range-node)))
                  (not(equal node-end (cdr range-node))))
          (setq result t))))
    result))

;; for treesit fold close all
(defun treesit-fold-range-c-like-comment (node offset)
  "Define fold range for C-like comemnt.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let ((text (treesit-node-text node)))
    (if (and (string-match-p "\n" text) (string-prefix-p "/*" text))
        (treesit-fold-range-block-comment node offset)
      (if (string-prefix-p "///" text)
          (my/treesit-fold-range-line-comment node offset "///")
        (my/treesit-fold-range-line-comment node offset "//")))))

(defun my/treesit-fold-range-line-comment (node offset prefix)
  (save-excursion
    (when-let* ((treesit-fold-line-comment-mode)  ; XXX: Check enabled!?
                (prefix-len (length prefix))
                (range-node (my/ts-node-range-continue node))
                (beg (+ (car range-node) prefix-len))
                (end (cdr range-node)))
      (treesit-fold--cons-add (cons beg end) offset))))

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
