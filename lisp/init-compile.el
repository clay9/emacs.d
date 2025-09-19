;;; init-compile.el --- Settings   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package compile
  :commands compile
  :bind ( :map compilation-mode-map
          ("g" . recompile)  ;; default
          ("n" . compilation-next-error)
          ("p" . compilation-previous-error)
          ("TAB" . compilation-display-error)
          ("RET" . compile-goto-error))
  :config
  (setq compilation-read-command t)

  :config
  (defun bury-compile-buffer-if-successful (buffer string)
    (if (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
	  (with-current-buffer buffer
	    (goto-char 1)
	    (search-forward "warning" nil t))))
        (run-with-timer 1 nil
		        (lambda (buf)
			  (bury-buffer buf)
			  (switch-to-prev-buffer (get-buffer-window buf) 'kill))
		        buffer)))
  (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful))

(provide 'init-compile)
;;; init-compile.el ends here
