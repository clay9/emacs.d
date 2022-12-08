(require 'linum)


;; ****************************************************
;; Overloaded Function
;; ****************************************************
(defun linum-update-window (win)
  "Used: when update window
   Change: add \"    \" (four space) after linum num
   Origin: Update line numbers for the portion visible in window WIN."
  (goto-char (window-start win))
  (let* ((line (line-number-at-pos))
	 (limit (window-end win t))
	 (fmt (cond ((stringp linum-format) linum-format)
		    ((eq linum-format 'dynamic)
		     (let ((w (length (number-to-string
				       (count-lines (point-min) (point-max))))))
		       (concat "%" (number-to-string w) "d")))))
	 ;;重载fmt, 增加空格, 使行号与代码分离开
	 (fmt-new (concat fmt "    "))
	 (width 0))
    (run-hooks 'linum-before-numbering-hook)
    ;; Create an overlay (or reuse an existing one) for each
    ;; line visible in this window, if necessary.
    (while (and (not (eobp)) (< (point) limit))
      (let* ((str (if fmt-new
                      (propertize (format fmt-new line) 'face 'linum)
                    (funcall linum-format line)))
             (visited (catch 'visited
                        (dolist (o (overlays-in (point) (point)))
                          (when (equal-including-properties
				 (overlay-get o 'linum-str) str)
                            (unless (memq o linum-overlays)
                              (push o linum-overlays))
                            (setq linum-available (delq o linum-available))
                            (throw 'visited t))))))
        (setq width (max width (length str)))
        (unless visited
          (let ((ov (if (null linum-available)
                        (make-overlay (point) (point))
                      (move-overlay (pop linum-available) (point) (point)))))
            (push ov linum-overlays)
            (overlay-put ov 'before-string
                         (propertize " " 'display `((margin left-margin) ,str)))
            (overlay-put ov 'linum-str str))))
      ;; Text may contain those nasty intangible properties, but that
      ;; shouldn't prevent us from counting those lines.
      (let ((inhibit-point-motion-hooks t))
        (forward-line))
      (setq line (1+ line)))
    (when (display-graphic-p)
      (setq width (ceiling
                   (/ (* width 1.0 (linum--face-width 'linum))
                      (frame-char-width)))))
    (set-window-margins win width (cdr (window-margins win)))))


(provide 'init-linum-mode)
