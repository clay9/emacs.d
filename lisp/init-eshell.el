(defun my/shell-mode-hook ()
  "Custom `shell-mode' behaviours."
  ;; Kill the buffer when the shell process exits.
  (let* ((proc (get-buffer-process (current-buffer)))
	 (sentinel (process-sentinel proc)))
    (set-process-sentinel
     proc
     `(lambda (process signal)
	;; Call the original process sentinel first.
	(funcall #',sentinel process signal)
	;; Kill the buffer on an exit signal.
	(and (memq (process-status process) '(exit signal))
	     (buffer-live-p (process-buffer process))
	     (kill-buffer (process-buffer process)))))))
(add-hook 'shell-mode-hook 'my/shell-mode-hook)

;; add path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))


(provide 'init-eshell)
