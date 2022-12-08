;; ****************************************************
;; compile  el->elc
;; ****************************************************
(defun my-recompile-all-el ()
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

;; ****************************************************
;; yank
;; ****************************************************
(setq yank-point 0)
(defun my-yank-pop()
  "Used by Fun:my-yank
  Fcuntion: yank-pop && record point"
  (yank-pop)
  (setq yank-point (point)))
(defun my-yank ()
  "Used by user, key: C-y
   Function: first call yank; then call yank-pop"
  (interactive)
  ;; after yank && position not change, call yank-pop
  (if (= (point) yank-point)
      (my-yank-pop)
    (yank)
    (setq yank-point (point))
    ))


;; ****************************************************
;; coding system
;; ****************************************************
(defun my-coding-system ()
  "Userd by user, M-x RET my-coding-system
   Function: first revert; then set coding-system utf-8"
  (interactive)
  ;(call-interactively 'revert-buffer-with-coding-system)
  (revert-buffer-with-coding-system 'gb2312)
  (set-buffer-file-coding-system 'utf-8-hfs)
  (save-buffer))

(provide 'init-basic-fun)
