

;; ****************************************************
;; 0.EDE
;; ****************************************************
(require 'ede)
(global-ede-mode 1)
;; 限定项目
(ede-cpp-root-project "qyserver_kernel"
		      :name "qyserver_kernel"
		      ;; 仅仅用来定位其他文件
		      :file "/Users/clay/qy/qy-server/kernel/Makefile"
		      ;; 自定义包含
		      :include-path '("/base"
				      "/define"
				      "/kernel"
				      "/tools")
		      ;; 系统头文件
		      ;;system-include-path '("~/linux")
		      )


;; ****************************************************
;; 1.semantic
;; ****************************************************
(require 'semantic)
(setq semantic-default-submodes '
      (global-semanticdb-minor-mode          ;; Maintain tag database. 
       global-semantic-idle-scheduler-mode   ;; Reparse buffer when idle.
       ;global-semantic-idle-summary-mode     ;; Show summary of tag at point.
       ;global-semantic-highlight-func-mode   ;; Highlight the current tag.
      ))
(semantic-mode 1)

;; db默认目录 TODO 为找到该var
;; semanticdb-default-save-directory

;; 添加系统path
;;(semantic-add-system-include)


;; 配置支持gnu global
;;(when (cedet-gnu-global-version-check t)
;;  (semanticdb-enable-gnu-global-databases 'c-mode)
;;  (semanticdb-enable-gnu-global-databases 'c++-mode))

;; 配置支持ctags Unix Shell、Perl、Pascal、Tcl、Fortran、Asm
;;(when (cedet-ectag-version-check t)
;;  (semantic-load-enable-primary-exuberent-ctags-support))


(provide 'init-cedet)
