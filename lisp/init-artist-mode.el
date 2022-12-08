(require 'artist)

;; open mode
;(artist-mode t);; C-h f RET artist-mode RET查看具体信息

;; 字符中 圆的比例 width/height
(setq artist-aspect-ratio  2.5)

;; hook
(defun my-artist-mode-hook()
  (linum-mode t) ;;增加line num的显示
  (setq tab-width 4
	indent-tabs-mode nil))
(add-hook 'artist-mode-hook 'my-artist-mode-hook)


;; ****************************************************
;; self-function 1
;; ****************************************************
(defun my-artist-mode-draw-line (&optional col_row length)
  "Used by hydra-artist-mode"
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
  "Used by hydra-artist-mode"
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
	   (artist-move-to-xy point_col (- point_row (/ row 2)))
	   ))

    (artist-key-set-point)
    (artist-forward-char col)
    (artist-next-line row)
    (artist-key-set-point)
    (artist-move-to-xy point_col point_row)
    ))

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

;; ****************************************************
;; self-function 2
;; ****************************************************
(defun my-artist-mode-get-rectangle ()
    "Used by my-artist-mode-cut-rectangle, my-artist-mode-kill-rectangle"

    ;; step0. check if in-rectangle TODO
    
    ; step1. find left-top '+'
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
    (artist-key-set-point))


    

(provide 'init-artist-mode)
