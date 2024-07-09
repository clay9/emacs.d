;;; init-text-move-and-kill.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; delete

;; target object: (see thing-at-point)
;; 1.char {backword, forword} => ignore char forword
;; 2.word
;; 3.sentence
;; 4.line {[point, line-end], whole-line}
;; 5.region

(defun my/delete/thing-at-point (thing)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if (not bounds) nil
      (delete-region (car bounds) (cdr bounds)) t)))

;; 优先级: 行首 => word => sexp
(defun my/delete()
  (interactive)
  (let* ((is-whitespace (and (equal 32 (char-after))
                             (equal 32 (char-before)))))
  (if (bolp)
      (my/delete/thing-at-point 'line)
    (if is-whitespace
        (progn (delete-all-space) (insert " "))
      (if (bounds-of-thing-at-point 'word)
          (my/delete/thing-at-point 'word)
        (if (bounds-of-thing-at-point 'sexp)
            (my/delete/thing-at-point 'sexp)
          nil))))))


;;; kill regeion

;; 因为mark不可见, 导致kill-region时候会有奇怪现象
;; 因此只对active-region进行kill, kill-save

;; active target object:
;; 1. word, sentence, sexp => my/expand-region, my/contract-region
;; 2. line => C-a, 按住shift, C-e, 释放shift. 进行选中

(defun my/kill()
  (interactive)
  (if (not (use-region-p))
      (error "no active region")
    (call-interactively 'kill-region)))

(defun my/kill-save()
  (interactive)
  (if (not (use-region-p))
      (error "no active region")
    (call-interactively 'kill-ring-save)))


;;; yank

;; yank => yank-pop[1-3) => consult-yank-from-kill-ring
(setq my/last-yank-point 0)
(setq my/yank-times 1)
(defun my/yank-pop ()
  (interactive)
  (let* ((yank-p (/= (point) my/last-yank-point))
	 (yank-pop-p (< my/yank-times 3)))
    (if yank-p
	(setq my/yank-times 1)
      (setq my/yank-times (1+ my/yank-times)))
    (if yank-p
	(yank)
      (if yank-pop-p
	  (yank-pop 1)
	(delete-region (mark t) (point))
	(call-interactively 'consult-yank-from-kill-ring))))
  (setq my/last-yank-point (point)))


;;; expand

(use-package expand-region
  :commands (er/expand-region er/contract-region))


(provide 'init-text-move-and-kill)
;;; init-text-move-and-kill.el ends here
