;;; init-site-lisp.el --- Support manually installed elisp in site-lisp -*- lexical-binding: t -*-

;; Add site-lisp and its immediate subdirs to `load-path`
(let ((site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory)))
  (when (file-directory-p site-lisp-dir)
    (let ((add-subdirs-to-load-path
           (lambda (parent-dir)
             "Add every non-hidden subdirectory of PARENT-DIR to `load-path`."
             (dolist (dir (directory-files parent-dir t "^[^.]" t))
               (when (file-directory-p dir)
                 (add-to-list 'load-path dir))))))

      ;; 添加 site-lisp 目录
      (add-to-list 'load-path site-lisp-dir)
      ;; 添加 site-lisp 的子目录
      (funcall add-subdirs-to-load-path site-lisp-dir))))

(provide 'init-site-lisp)
;;; init-site-lisp.el ends here
