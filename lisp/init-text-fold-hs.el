;;; init-text-fold-hs.el --- Text folding: hs-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; This file provides commands and keybindings for folding/hiding text:
;;;   - hideshow minor mode for text
;;; Code:

;; ----------------------
;;; hideshow minor mode
;; ----------------------
(use-package hideshow
  :diminish hs-minor-mode
  :config
  (defvar text/hs-hide-all nil)

  (defun text/hs-shift-tab ()
    "Context-aware Shift-TAB for hideshow."
    (cond ((hs-inside-comment-p) (hs-toggle-hiding))
          ((text/hs-inside-block-p) (text/hs-toggle-block))
          (t (text/hs-toggle-all))))

  (defun text/hs-toggle-all ()
    "Toggle hide/show all blocks."
    (setq text/hs-hide-all (not text/hs-hide-all))
    (if text/hs-hide-all (hs-hide-all) (hs-show-all)))

  (defun text/hs-toggle-block ()
    "Toggle hide/show current block or level."
    (if (hs-already-hidden-p) (hs-show-block)
      (if (text/hs-is-have-level) (hs-hide-level 1)
        (hs-hide-block))))

  (defun text/hs-is-have-level ()
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

  (defun text/hs-inside-block-p ()
    "Check if point is inside a foldable block."
    (save-excursion
      (or (hs-looking-at-block-start-p)
          (hs-find-block-beginning)
          (hs-inside-comment-p)))))

(provide 'init-text-fold-hs)
;;; init-text-fold-hs.el ends here
