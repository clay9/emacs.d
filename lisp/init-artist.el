;;; init-artist.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package artist
  :commands artist-mode
  :bind ( :map artist-mode-map
          ("C-<return>" . org-newline-and-indent)
          ("C-k" . org-kill-line)
          ("C-j" . transient/artist-mode))
  :config
  (transient-define-prefix transient/artist-mode()
    [[:class transient-column "shape"
	     ("l" "line" my-artist-mode-draw-line)
	     ("p" "custom line" my-artist-mode-draw-custom-line)
	     ("r" "rectangle" my-artist-mode-draw-rectangle)
	     ("e" "ellipse" my-artist-mode-draw-ellipse)]
     
     [:class transient-column "action"
	     ("w" "cut rectangle" my-artist-mode-cut-rectangle)
	     ("k" "kill rectangle" my-artist-mode-erase-rectangle)
	     ("y" "yank" my-artist-mode-paste)]])

  :config
  (setq artist-aspect-ratio  2.5
        tab-width 4
        indent-tabs-mode nil)


  (defun my-artist-mode-draw-line (&optional col_row length)
    (interactive "Ncol(1), row(2): \nNlength: ")
    (artist-select-op-line)
    (let ((point_col (artist-current-column))
	  (point_row (artist-current-line)))
      (artist-key-set-point)
      (if (= col_row 1)
	  (artist-forward-char (- length 1))
        (artist-next-line (- length 1)))
      (artist-key-set-point)
      (artist-move-to-xy point_col point_row)))

  (defun my-artist-mode-draw-custom-line ()
    (interactive)
    (artist-select-op-line)
    (artist-key-set-point))

  (defun my-artist-mode-draw-rectangle (&optional pos col row)
    "Used by hydra-artist-mode"
    (interactive "Ntop-left(1), top-center(2), left-center(3): \nNcol: \nNrow:")
    (artist-select-op-rectangle)
    (let ((point_col (artist-current-column))
	  (point_row (artist-current-line))
	  (col (- col 1))
	  (row (- row 1)))
      (cond ((= pos 1) ); do nothing
	    ((= pos 2)
	     (artist-move-to-xy (- point_col (/ col 2)) point_row))
	    ((= pos 3)
	     (artist-move-to-xy point_col (- point_row (/ row 2)))))
      (artist-key-set-point)
      (artist-forward-char col)
      (artist-next-line row)
      (artist-key-set-point)
      (artist-move-to-xy point_col point_row)))

  ;; TODO 该函数没有经过测试, 好像真实的半径应该是col-1, row-1才对
  (defun my-artist-mode-draw-ellipse (&optional col row)
    "Used by hydra-artist-mode"
    (interactive "Ncol_r: \nNrow_r:")
    (artist-select-op-ellipse)
    (let ((point_col (artist-current-column))
	  (point_row (artist-current-line)))
      (artist-key-set-point)
      (artist-forward-char col)
      (artist-next-line row)
      (artist-key-set-point)
      (artist-move-to-xy point_col point_row)))

  (defun my-artist-mode-get-rectangle ()
    "Used by my-artist-mode-cut-rectangle, my-artist-mode-kill-rectangle"
    ;; step0. check if in-rectangle TODO
    ;; step1. find left-top '+'
    ;; find '|' or '+'
    (while (and (not (eq ?| (char-after)))
		(not (eq ?+ (char-after))))
      (artist-backward-char 1))

    ;; if '|', find top '+'
    (when (eq ?| (char-after))
      (while (not (eq ?+ (char-after)))
	(artist-previous-line 1)))

    ;; if right '+' => refind left '+'
    (when (and (eq ?+ (char-after))
	       (eq ?- (char-before)))
      (artist-backward-char 1)
      (while (not (eq ?+ (char-after)))
	(artist-backward-char 1)))

    ;; if bottom '+' => refind top '+'
    (when (eq ?+ (char-after))
      (artist-previous-line 1)
      (if (eq ?| (char-after))
	  (while (not (eq ?+ (char-after)))
	    (artist-previous-line 1))
	(artist-next-line 1)))

    ;; store x,y
    (setq artist_rectangle_start_col (artist-current-column))
    (setq artist_rectangle_start_row (artist-current-line))

    ;; step2. find right-bottom '+'
    ;; find right-top '+'
    (artist-forward-char 1)
    (while (not (eq ?+ (char-after)))
      (artist-forward-char 1))

    ;; find right-bottom '+'
    (artist-next-line 1)
    (while (not (eq ?+ (char-after)))
      (artist-next-line 1))
    (cons artist_rectangle_start_col artist_rectangle_start_row))

  (defun my-artist-mode-cut-rectangle ()
    "Used by hrdra-artist-mode"
    (interactive)
    (let ((col_row (my-artist-mode-get-rectangle)))
      (artist-select-op-cut-rectangle)
      (artist-key-set-point)
      (artist-move-to-xy (car col_row) (cdr col_row))
      (artist-key-set-point)))

  (defun my-artist-mode-erase-rectangle ()
    "Used by hrdra-artist-mode"
    (interactive)
    (let ((col_row (my-artist-mode-get-rectangle)))
      (artist-select-op-erase-rectangle)
      (artist-key-set-point)
      (artist-move-to-xy (car col_row) (cdr col_row))
      (artist-key-set-point)))

  (defun my-artist-mode-paste ()
    "Used by hrdra-artist-mode"
    (interactive)
    (artist-select-op-paste)
    (artist-key-set-point)))

(provide 'init-artist)
;;; init-artist.el ends here
