;;; init-compile.el --- Settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package compile
  :commands compile
  :bind (:map compilation-mode-map
              ("n" . compilation-next-error)
              ("p" . compilation-previous-error)
              ("TAB" . compilation-display-error)
              ("RET" . compile-goto-error))
  :config
  ;; 每次执行 compile 都提示输入命令
  (setq compilation-read-command t)

  ;;----------------------------------------
  ;;; 自动隐藏成功的编译缓冲区
  ;;----------------------------------------
  (defun bury-compile-buffer-if-successful (buffer string)
    "Automatically bury BUFFER if the compilation finished successfully with no warnings.
STRING is the compilation finished message."
    (with-current-buffer buffer
      (let ((has-warning (search-forward "warning" nil t)))
        (cond
         ;; 编译成功且没有 warning → bury 并提示
         ((and (string-match "finished" string)
               (not has-warning))
          (run-with-timer
           1 nil
           (lambda (buf)
             (bury-buffer buf)
             (let ((win (get-buffer-window buf)))
               (when win
                 (switch-to-prev-buffer win 'kill))))
           buffer)
          (message "Compilation finished successfully! Buffer buried."))
         ;; 编译成功但有 warning → 提示
         ((and (string-match "finished" string)
               has-warning)
          (message "Compilation finished with warnings."))
         ;; 编译失败 → 提示失败
         (t
          (message "Compilation failed."))))))

  (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful))

(provide 'init-compile)
;;; init-compile.el ends here
