;;; sub-text-fold-treesit.el --- Text folding: treesit -*- lexical-binding: t -*-
;;; Commentary:
;; This file provides commands and keybindings for folding/hiding text:
;;   - Tree-sitter based folding for syntax-aware hiding
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
  (treesit-fold-line-comment-mode t)

  (defun text/treesit-fold-cycle()
    "Org-style cycle for treesit fold.
- If entry is folded, show its body and subtree(folded).
- If entry has subtree and subtree is fully folded, expand the whole subtree.
- Otherwise, fold the entry."
    (let* (;; 当前pos所在的最小的可折叠node. nil表示没有
           (node (treesit-fold--foldable-node-at-pos))
           ;; node的ov标志
           (ov   (and node (treesit-fold-overlay-at node)))
           ;; node的一级child
           (child-list (when node (treesit-node-children node)))
           ;; treesit-fold--non-foldable-node-p 判断node是否可折叠的规则
           (mode-ranges (alist-get major-mode treesit-fold-range-alist))
           ;; 0:没有可折叠subtree; 1:有可折叠subtree 且 subtree都已折叠; -1:有可折叠subtree未折叠
           (all-fold 0)
           ;; 获取真实的all-fold值
           (_ (cl-block text/treesit-block
                (dolist (child child-list)
                  (let* ((foldable (not (treesit-fold--non-foldable-node-p child mode-ranges)))
                         (ov (and foldable (treesit-fold-overlay-at child))))
                    (when foldable
                      ;; 第一次遇到可折叠的subtree的时候, set foldable
                      (when (= all-fold 0) (setq all-fold 1))
                      ;; 遇到未折叠的subtree (foldable=t and ov=nil)
                      (when (not ov)
                        (setq all-fold -1)
                        (cl-return-from text/treesit-block 0)))))))
           (root (treesit-buffer-root-node)))
      (cond
       ((not node)
        (cl-labels ((any-folded-p (node)
                      (or (treesit-fold-overlay-at node)
                          (cl-some #'any-folded-p (treesit-node-children node)))))
          (if (any-folded-p root)
              ;; 0.1 node外. 有任何折叠的node, 则展开所有
              (treesit-fold-open-all)
            ;; 0.2 node外. 所有node都已展开, 则关闭所有
            (treesit-fold-close-all))))
       ;; 1. 当前 entry 折叠 → 展开 entry body 和 children(folded)
       (ov
        (treesit-fold-open)
        (mapc #'treesit-fold-close child-list))
       ;; 2. 如果有subtree 且 subtree都已折叠 → 展开整个 subtree
       ((= all-fold 1)
        (treesit-fold-open-recursively))

       ;; 3. 其他情况 → 折叠entry
       (t
        (treesit-fold-close))))))

(provide 'sub-text-fold-treesit)
;;; sub-text-fold-treesit.el ends here
