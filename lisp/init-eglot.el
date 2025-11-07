;;; init-eglot.el --- LSP support via Eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :commands (eglot eglot-ensure)
  :config
  ;; 提高进程输出缓冲区大小，避免大项目下的性能瓶颈
  (setq read-process-output-max (* 1024 1024))  ;; 1MB

  ;; 避免暴露进程 ID（部分系统下的安全优化）
  (setq eglot-withhold-process-id t)

  ;; 关闭 Eglot 的 JSON RPC 日志，减少干扰
  (advice-add 'jsonrpc--log-event :override #'ignore)

  ;; 禁止 Eglot 事件缓冲区累积数据（更安静、更节省资源）
  (setopt eglot-events-buffer-size 0)

  ;;----------------------------------------
  ;;; LSP Server 配置: C++
  ;;----------------------------------------
  (add-to-list 'eglot-server-programs
               '(c++-ts-mode . ("clangd"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--limit-results=500"))))

(provide 'init-eglot)
;;; init-eglot.el ends here
