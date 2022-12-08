
;; 保存之前, cleanup whitespace
(add-to-list 'before-save-hook (lambda ()
				 (whitespace-cleanup)))

(provide 'init-whitespace-mode)
