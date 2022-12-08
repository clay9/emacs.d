;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(let ((minver "28"))
  (when (version< emacs-version minver)
    (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible. his config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/fun" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time


;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Bootstrap config


(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages


(defconst my/gtd-dir  (if (eq system-type 'windows-nt) "~/gtd/" "~/my/gtd/"))
(defconst my/ecfg-dir (concat my/gtd-dir ".emacs_cfg/"))


(require 'init-theme)
(require 'init-frame)           ;;窗口初始化 （包含彩虹猫nyan)
(require 'init-font)            ;;字体初始化
(require 'init-encode)          ;;字符编码
(require 'init-linum-mode)      ;;行号显示
(require 'init-display-buffers) ;;buffer显示

;; 内在优化 (均是全局生效)
(require 'init-helm-mode)       ;;Helm
(require 'init-company-mode)    ;;自动补全
(require 'init-yasnippet-mode)  ;;模板文件
(require 'init-search)          ;;搜索替换
(require 'init-whitespace-mode) ;;移除多余空格

(require 'init-symbol-overlay)  ;;符号选择
(require 'init-expand-region)   ;;sexp选择
(require 'init-smartparens-mode);;sexp操作 (移动, wrap, unwrap)
(require 'init-hs-minor-mode)   ;;sexp折叠
(require 'init-windows)         ;;窗口选择

;; 常用功能
(require 'init-bookmark)        ;;书签
(require 'init-eshell)          ;;内置命令行
(require 'init-tramp)           ;;tramp
(require 'init-artist-mode)     ;;ascii graph
(require 'init-basic-fun)       ;;自定义函数(basic)


;; *************************
;; 日程排表 && 知识管理 (GTD)
;; *************************
(require 'init-calendar)        ;;calendar (org-agenda need func)
(require 'init-org-mode)        ;;org
(require 'init-org-agenda-mode) ;;org-agenda


;; *************************
;; project manager
;; *************************
(require 'init-magit)           ;;magit
(require 'init-project)         ;;项目管理 (有调用magit func)


;; *************************
;; c-comon development (IDE)
;; *************************
;; *******1.编辑器********
;; 1. TAG索引  : helm-gtags   + global
;; 2. 自动补全 : company-mode + clang
(require 'init-cc-mode)        ;;cc样式(major)
(require 'init-helm-gtags)     ;;TAG索引
(require 'init-flycheck-mode)  ;;实时语法检查
(require 'init-sr-speedbar)    ;;文件目录列表
;(require 'init-cedet)          ;;CEDET (emacs development env tools) ;;问题太多, 不是很好用

;; *******2.编译器********
(require 'init-compile)        ;;编译器
;; *******3.调试器********
(require 'init-gdb)            ;;gdb调试器


;; *************************
;; lisp development (IDE)
;; *************************
;; ********编译器********
(require 'init-slime)


;; *************************
;; other major
;; *************************
(require 'init-yaml-mode)


;; *************************
;; 快捷键
;; *************************
(require 'init-disable-mouse)   ;;禁用鼠标快捷键
(require 'init-local-shortkey)  ;;局部快捷键
(require 'init-global-shortkey) ;;全局快捷键


;; *************************
;; Ohters
;; *************************
;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))
