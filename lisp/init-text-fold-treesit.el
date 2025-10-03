;;; init-text-fold-treesit.el --- Text folding: treesit -*- lexical-binding: t -*-
;;; Commentary:
;;; This file provides commands and keybindings for folding/hiding text:
;;;   - Tree-sitter based folding for syntax-aware hiding
;;; Code:

;; ----------------------
;;; Tree-sitter folding
;; ----------------------
(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold.git" :rev :newest)
  :config
  ;; 折叠时候, 显示函数
  (setq treesit-fold-line-count-show t)
  (setq treesit-fold-line-count-format " <%d lines> ")

  (global-treesit-fold-mode t)
  ;; 折叠单行注释 (需要多个单行注释在一起)
  (treesit-fold-line-comment-mode t))

(defun text/ts-shift-tab ()
  "Context-aware Shift-TAB for tree-sitter folding."
  (let ((in-multi-comment (text/ts-in-multi-comment))
        (node (treesit-fold--foldable-node-at-pos)))
    (cond
     (in-multi-comment (text/ts-toggle-comment))
     (node (text/ts-toggle))
     (t (text/ts-toggle-all)))))

(defun text/ts-node-range-continue (node)
  "Get continuous node range for folding."
  (let* ((get-node (lambda (node next)
                     (let ((cnode node) result)
                       (while (and cnode (equal (treesit-node-type node)
                                                (treesit-node-type cnode)))
                         (setq result cnode)
                         (setq cnode (if next
                                         (treesit-node-next-sibling cnode)
                                       (treesit-node-prev-sibling cnode))))
                       result)))
         (first-node (funcall get-node node nil))
         (last-node (funcall get-node node t)))
    (cons (treesit-node-start first-node)
          (treesit-node-end last-node))))

(defun text/ts-in-multi-comment ()
  "Check if point is in a multi-line comment."
  (let ((node (treesit-node-at (point)))
        result)
    (when (equal (treesit-node-type node) "comment")
      (let ((range-node (text/ts-node-range-continue node))
            (node-start (treesit-node-start node))
            (node-end (treesit-node-end node)))
        (when (or (not (equal node-start (car range-node)))
                  (not (equal node-end (cdr range-node))))
          (setq result t))))
    result))

(defun text/ts-toggle-comment ()
  "Toggle folding of multi-line comment."
  (treesit-fold-line-comment-mode)
  (let* ((node (treesit-node-at (point)))
         (prefix "//")
         (prefix-len (length prefix))
         (node-range (text/ts-node-range-continue node))
         (range (cons (+ (car node-range) prefix-len)
                      (cdr node-range))))
    (mapc #'delete-overlay (treesit-fold--overlays-in 'invisible 'treesit-fold
                                                      (car range) (cdr range)))
    (run-hooks 'treesit-fold-on-fold-hook)))

(defun text/ts-toggle ()
  "Toggle folding of node at point."
  (let ((node (treesit-fold--foldable-node-at-pos))
        (ov (treesit-fold-overlay-at (treesit-fold--foldable-node-at-pos))))
    (if ov
        (treesit-fold-open-recursively)
      (text/ts-hide))))

(defun text/ts-hide ()
  "Hide child nodes of the current foldable node."
  (let ((node (treesit-fold--foldable-node-at-pos))
        (pos (point))
        act)
    (dolist (son (treesit-node-children node))
      (dolist (grandson (treesit-node-children son))
        (let* ((ov (treesit-fold-overlay-at grandson))
               (range (treesit-fold--get-fold-range grandson))
               (ov2 (treesit-fold--create-overlay range)))
          (unless (equal ov ov2)
            (run-hooks 'treesit-fold-on-fold-hook)
            (setq act t)))))
    (goto-char pos)
    (unless act (treesit-fold-close node))))

(defvar text/ts-hide-all nil)
(defun text/ts-toggle-all ()
  "Toggle hide/show all tree-sitter nodes."
  (setq text/ts-hide-all (not text/ts-hide-all))
  (if text/ts-hide-all
      (treesit-fold-close-all)
    (treesit-fold-open-all)))

(provide 'init-text-fold-treesit)
;;; init-text-fold-treesit.el ends here
