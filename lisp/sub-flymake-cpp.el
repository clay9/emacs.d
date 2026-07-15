;;; sub-flymake-cpp.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; TODO 后续删除cpplint, 使用clang-tidy代替

(require 'flymake)

(defconst flymake/cpplint-prog (or (executable-find "cpplint.py")
                                   "/usr/local/bin/cpplint.py"))

(defconst flymake/cpplint-args '("--verbose=0"
                                 "--filter=-legal/copyright,-build/header_guard,-build/include_subdir"))


(defun flymake/cpplint-diagnostic (report-fn &rest _args)
  (let* ((src-buf (current-buffer))
         (proc-buf (generate-new-buffer " *flymake-cpplint-output*"))
         (cmd (append (list flymake/cpplint-prog) flymake/cpplint-args '("-")))
         (proc (make-process
                :name "flymake-cpplint"
                :buffer proc-buf
                :command cmd
                :connection-type 'pipe
                :stderr proc-buf ;; 极其重要：cpplint 的全部警告都走 stderr
                :sentinel
                (lambda (p _event)
                  (when (eq (process-status p) 'exit)
                    (let (diags)
                      (unwind-protect
                          (when (and (buffer-live-p proc-buf) (buffer-live-p src-buf))
                            (with-current-buffer proc-buf
                              (goto-char (point-min))
                              ;; 增强版正则：完美匹配 `-:10: msg` 或 `filename:10: msg`
                              (while (re-search-forward "^\\(?:-\\|.\\+?\\):\\([0-9]+\\):\\s-*\\(.*\\)$" nil t)
                                (let ((line (string-to-number (match-string 1)))
                                      (msg (match-string 2)))
                                  (with-current-buffer src-buf
                                    ;; 严格限幅行号，防止空文件越界
                                    (let* ((max-l (line-number-at-pos (point-max)))
                                           (safe-l (min (max 1 line) max-l))
                                           (loc (flymake-diag-region src-buf safe-l))
                                           (diag (flymake-make-diagnostic
                                                  src-buf (car loc) (cdr loc) :warning msg)))
                                      (push diag diags)))))))

                        ;; 彻底清理进程输出缓冲区
                        (when (buffer-live-p proc-buf) (kill-buffer proc-buf))

                        ;; 核心修复：只有当 Flymake 认为当前回调未过期时，才上报结果
                        (if (and (functionp 'flymake-report-fn-obsolete-p)
                                 (flymake-report-fn-obsolete-p report-fn))
                            (message "【Flymake Cpplint】: 略过过期的后台检查上报。")
                          (funcall report-fn diags)))))))))

    ;; 异步向进程灌入当前 Buffer 数据(不管存没保存)
    (process-send-region proc (point-min) (point-max))
    (process-send-eof proc)
    ;; 必须返回进程对象告诉 Flymake 正在运行
    proc))

(defun flymake/cpplint-setup ()
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'flymake/cpplint-diagnostic nil t)
  (flymake-mode 1))

;; 绑定到 C/C++ 激活 Hook
(add-hook 'c++-mode-hook #'flymake/cpplint-setup)
(add-hook 'c++-ts-mode-hook #'flymake/cpplint-setup)

(provide 'sub-flymake-cpp)
;;; sub-flymake-cpp.el ends here
