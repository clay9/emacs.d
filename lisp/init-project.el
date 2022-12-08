(require 'project)

;; set project-list-file directoy
(setq project-list-file (expand-file-name "projects" my/ecfg-dir))

;; switch project without menu
(setq project-switch-commands 'project-find-file)

;; ****************************************************
;; my/func
;; ****************************************************
;; project.el中的这里有bug, 只能自定义一个
(defun my/project-switch-to-buffer()
  (interactive)
  (let* ((pr (project-current t))
	 (buffers (project-buffers pr))
	 buffer-names
	 (predicate
	  (lambda (buffer)
	    ;; buffer in project-buffers && not start-with \\*
	    (and (memq buffer buffer-names)
		 (not (string-match-p "\\*.*\\*" buffer))))))
    ;; helm-mode返回的是buffer-name(string), 因此predicate中应该判断buffer-name
    (dolist (buf buffers)
      (push (buffer-name buf) buffer-names))
    (let ((b (read-buffer  "Switch to Project buffer: " nil nil predicate) ))
      (switch-to-buffer b))))

(provide 'init-project)
