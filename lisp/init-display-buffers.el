;;; ctrl how buffer display
(setq display-buffer-alist
      '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\)\\*"
	 (display-buffer-reuse-mode-window  display-buffer-same-window)
	 (inhibit-same-window . nil)
	 (mode . help-mode))
	("\\*e?shell\\*"
	 (display-buffer-in-side-window)
	 (window-height . 0.4)
	 (side . bottom)
	 (slot . -1)
	 (window-parameters . ((no-other-window . t)
			       (no-delete-other-windows . t))))
	))


(provide 'init-display-buffers)
