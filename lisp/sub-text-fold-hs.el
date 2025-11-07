;;; sub-text-fold-hs.el --- Text folding: hs-mode -*- lexical-binding: t -*-
;;; Commentary:
;; This file provides commands and keybindings for folding/hiding text:
;;   - hideshow minor mode for text
;;; Code:

(use-package hideshow
  :diminish hs-minor-mode
  :ensure nil
  :config
  ;; ----------------------
  ;;; hideshow minor cycle
  ;; ----------------------
  (defun text/hs-cycle ()
    "Org-style cycle for outline mode.
- If entry is folded, show its body and subtree(folded).
- If entry has subtree and subtree is fully folded, expand the whole subtree.
- Otherwise, fold the entry."
    (cond
     ;; 0. 如果在注释中, 则toggle comment
     ((hs-inside-comment-p) (hs-toggle-hiding))
     ;; 1. 不在entry中. toggle all
     ((save-excursion
        (not (or (hs-looking-at-block-start-p)
                 (hs-find-block-beginning)
                 (hs-inside-comment-p))))
      (let ((any-hidden
             (cl-some (lambda (ov)
                        (and (overlay-get ov 'hs)
                             (overlay-get ov 'invisible)))
                      (overlays-in (point-min) (point-max)))))
        (if any-hidden
            ;; 1.1 node外. 有任何折叠的node, 则展开所有
            (hs-show-all)
          ;; 1.2 node外. 所有node都已展开, 则关闭所有
          (hs-hide-all))))
     ;; 2. 在entry中
     (t
      (cond
       ;; 2.1 当前 entry 折叠 → 展开 entry body 和 children(folded)
       ((save-excursion
          (hs-find-block-beginning)
          (funcall hs-forward-sexp-func 1)
          ;; hs-forward-sexp-func 会移动到block pos + 1的位置;
          ;; 而ov在block pos -1的位置. 所以这里需要(point) - 2
          (hs-overlay-at (- (point) 2)))
                                        ; ; 不在 Echo area 显示消息
        (let ((inhibit-message t)
              (message-log-max nil))
          ;; 只显示top children
          (hs-hide-level 0)))
       ;; 2.2 如果有subtree 且 subtree都已折叠 → 展开整个 subtree
       ((text/has-subtreee-all-folded)
        (save-excursion
          (hs-show-block)))
       ;; 2.3 其他情况 → 折叠entry
       (t
        (save-excursion
          (hs-hide-block))
        )))))

  (defun text/has-subtreee-all-folded ()
    ;; 结束的时候 goto here
    (setq here (point))
    ;; 0:没有可折叠subtree; 1:有可折叠subtree 且 subtree都已折叠; -1:有可折叠subtree未折叠
    (setq all-folded 0)
    ;; get block min,max
    (when (hs-find-block-beginning)
      (setq minp (1+ (point)))
      (funcall hs-forward-sexp-func 1)
      (setq maxp (1- (point))))
    (goto-char minp)
    (while (and (/= all-folded -1)
                (< (point) maxp)
                (progn
                  ;; 跳过注释
                  (forward-comment (buffer-size))
                  ;; 跳转到下一个block (subtree)
                  (re-search-forward hs-block-start-regexp maxp t)))
      ;; 当前 point 不在字符串或注释里
      (when (save-match-data (not (nth 8 (syntax-ppss))))
        ;; 跳到当前block的结尾处.
        ;; (因为re-search-foward的原因, 这里肯定能找到hs-find-block-beginning)
        ;; 下次re-search-forward找block就会还是寻找top children block.
        ;; 跳过二级, 三级.. child
        (hs-find-block-beginning)
        (setq child-min (1+ (point)))
	(funcall hs-forward-sexp-func 1)
        (setq child-max (1+ (point)))

        (let* ((one-line (<= (count-lines child-min child-max) 1))
               (ov (hs-overlay-at (- (point) 2))))
          ;; 非一行 或者 已经折叠了 就表示可折叠的subtree
          (when (or (not one-line) ov)
            (setq all-folded 1))
          ;; 检测是否为folded. 如果有ov表示折叠; 如果只有一行,也表示折叠
          (unless (or ov one-line)
            ;; (message "get next block. ov is nil. point:%s" (point))
            (setq all-folded -1)))))
    (goto-char here)
    ;; (message "all-folded: %s" all-folded)
    (= all-folded 1))

  ;; ----------------------
  ;;; fix hs-show-block
  ;; ----------------------
  ;; 'hs-show-block' 会导致第二个及后续的top child无法展开
  (defun text/hs-show-block (&optional end)
    "Show current block and all its top-level children.
With prefix argument END non-nil, reposition point at block end.
Runs `hs-show-hook' after expanding."
    (interactive "P")
    (when (hs-find-block-beginning)
      (let* ((block-start (point))
             (block-end (progn (funcall hs-forward-sexp-func 1)
                               (1- (point)))))
        ;; 删除父 block overlay
        (hs-discard-overlays block-start block-end)

        ;; 遍历父 block内部，展开所有 top-level子 block overlay
        (goto-char block-start)
        (while (and (< (point) block-end)
                    (re-search-forward hs-block-start-regexp block-end t))
          ;; 跳过注释/字符串
          (forward-comment (buffer-size))
          (when (not (nth 8 (syntax-ppss)))
            (let ((ov (hs-overlay-at (point))))
              (when ov
                (delete-overlay ov)))))

        ;; 调整光标
        (goto-char (if end block-end (1+ block-start)))

        ;; 触发钩子
        (run-hooks 'hs-show-hook))))
  (fset 'hs-show-block #'text/hs-show-block))

(provide 'sub-text-fold-hs)
;;; sub-text-fold-hs.el ends here
