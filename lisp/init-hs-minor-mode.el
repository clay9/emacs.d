(require 'hideshow)

;; hook
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'sh-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;; ****************************************************
;; Function
;; ****************************************************
(defun my-hs-shift-tab ()
  "Used by user;
   Fcuntion: 函数外, 隐藏显示全部
	     函数内, 隐藏sub, 没有则隐藏自己; 已经隐藏则显示自己和sub"
  (interactive)
  (if (my-hs-is-in-block)
      (my-hs-toggle-block)
    (my-hs-toggle-all)))

(defvar my-hs-hide-all nil)
(defun my-hs-toggle-all ()
  "toggle hide | show all"
  (setq my-hs-hide-all (not my-hs-hide-all))
  (if my-hs-hide-all
      (hs-hide-all)
    (hs-show-all)))

(defun my-hs-toggle-block ()
  "toggle hide|show [block, level]"
  (if (my-hs-is-have-level)
      (hs-hide-level 1)

    ;; dont use hs-toggle-hiding, it's have bugs
    (if (hs-already-hidden-p)
	(hs-show-block)
     (hs-hide-block))))

(defun my-hs-is-have-level ()
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
      (if (hs-already-hidden-p)
	  ()
	(setq have-level t))))
  (goto-char here)
  (if have-level
      t
    nil))

(defun my-hs-is-in-block ()
  "check is in block"
  (setq here (point))
  (setq in-block nil)
  (if (or (hs-looking-at-block-start-p)
	  (hs-find-block-beginning))
      (setq in-block t))
  (goto-char here)
  (if in-block
      t
    nil))


(provide 'init-hs-minor-mode)
