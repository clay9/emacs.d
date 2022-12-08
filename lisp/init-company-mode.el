(require-package 'company)
(require 'company)
(require-package 'company-c-headers)
(require 'company-c-headers)

;; ****************************************************
;; company-mode
;; ****************************************************
;; start up company-mode
(add-hook 'after-init-hook 'global-company-mode)

;; chose backends
;; company-mode根据list顺序选择第一个可行的
;; 1. company-semantic 详见CEDET
;; 2. company-clang 依赖外部程序clang
;; 3. company-gtags 依赖外部程序gtags生成的file (GTAGS, GRTAGS, GPATH)
;;    company-gtags表现不好, 其未区分scopes(作用域)
;; 这里建议使用clang
;;
;; 使用clang, 去除semantic, 因为其优先级比clang高, 详见 C-h v company-backends
;(setq company-backends (delete 'company-semantic company-backends))       

;; ****************************************************
;; company-c-headers
;; ****************************************************
;; include-system-path
(setq company-c-headers-path-system
   (quote
    ("/usr/include/" "/usr/local/include/" "/usr/include/c++/5.4.0/"
     "/Library/Developer/CommandLineTools/usr/include/c++/v1/" )))     
(add-to-list 'company-backends 'company-c-headers)


(provide 'init-company-mode)
