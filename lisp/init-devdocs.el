;;; init-devdocs.el --- DevDocs integration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package devdocs
  :config
  ;; 设置 Devdocs 数据目录
  (setq devdocs-data-dir (concat my/ecfg-dir "devdocs"))
  ;; 打开文档时自动选择窗口
  (setq devdocs-window-select t)

  ;; 复用窗口或使用已选择的窗口显示 Devdocs buffer
  (add-to-list 'display-buffer-alist
               `("\\*devdocs" (display-buffer-reuse-window display-buffer-same-window)))

  ;; 自动安装 C++ 文档
  (let ((cpp-doc (concat devdocs-data-dir "/cpp")))
    (unless (file-exists-p cpp-doc)
      (devdocs-install "cpp"))))

(provide 'init-devdocs)
;;; init-devdocs.el ends here
