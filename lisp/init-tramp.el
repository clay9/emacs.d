(require 'tramp)

;; 默认连接存储文件, nil表示禁用持久性存储
(setq tramp-persistency-file-name (concat my/ecfg-dir "tramp"))

;; windows下tramp使用plink取代ssh
(when (eq system-type 'windows-nt)
  (setq exec-path (cons "E:/putty" exec-path))
  (setq tramp-default-method "plink")
  )

(provide 'init-tramp)
