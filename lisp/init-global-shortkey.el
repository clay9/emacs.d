(require 'hydra) ;;使用hydra管理快捷键

;; 目录一栏
;; 1.对象操作
;;   - symbol-overlay-mode
;;   - smartparens-mode
;; 2.GTD
;; 3.Project && magit
;; 4.others
;;   - C-x C-e


;; ****************************************************
;; 1. 对象操作
;; ****************************************************
(defun my/if-in-symbol-mark ()
  "Used by hydra-search. when-in-symbol-mark return t, or nil"
  (let* ((symbol (or (thing-at-point 'symbol) nil))
	 (keyword (symbol-overlay-assoc symbol)))
    keyword))
(defhydra hydra-search (:color blue :hint nil)
  "
^symbol^          ^sexp^          ^region^          ^search^          ^yas^
^^^^^^------------------------------------------------------------------------------
_i_: put          _b_: begin      _c_: !comment     _a_: all          _C-i_: insert
_p_: previous     _e_: end        ^ ^               _s_: isearch      _C-v_: view
_n_: next         _p_: previous   ^ ^               _t_: replace      ^ ^
_t_: replace      _n_: next       ^ ^               _g_: go line      ^ ^
^ ^               _m_: wrap       ^ ^               ^ ^               ^ ^
^ ^               _u_: unwrap     ^ ^               ^ ^               ^ ^
^ ^               _r_: rewrap     ^ ^               ^ ^               ^ ^
"
  ("i" symbol-overlay-put)
  ("p" (funcall (lambda()
		  (interactive)
		  (if (my/if-in-symbol-mark)
		      (symbol-overlay-jump-prev)
		    (sp-backward-sexp) ))))
   ("n" (funcall (lambda()
		  (interactive)
		  (if (my/if-in-symbol-mark)
		      (symbol-overlay-jump-next)
		    (sp-forward-sexp) ))))
   ("t" (funcall (lambda()
		  (interactive)
		  (if (my/if-in-symbol-mark)
		      (symbol-overlay-rename)
		    (replace-string) ))))

   ("c" comment-or-uncomment-region)

   ("b" sp-beginning-of-sexp)
   ("e" sp-end-of-sexp)
   ;;("p")
   ;;("n")
   ("m" sp-wrap-round)
   ("u" sp-unwrap-sexp)
   ("r" sp-rewrap-sexp)

   ("a" helm-occur)
   ("s" isearch-forward)
   ;;("t" replace-string)
   ("g" goto-line)

   ("C-i" yas-new-snippet)
   ("C-v" yas-visit-snippet-file))
(global-set-key (kbd "C-s") 'hydra-search/body)

;; sexp region
(global-set-key (kbd "C-w") 'kill-region)         ;;kill
(global-set-key (kbd "C-y") 'my-yank)             ;;yank && yank-pop
(global-set-key (kbd "C-=") 'my-expand-region)    ;;choose+
(global-set-key (kbd "C--") 'my-contract-region)  ;;choose-

;; buffer, file, bookmarks
(global-set-key (kbd "C-x f")   'helm-find-files)
(global-set-key (kbd "C-x m")   'helm-bookmarks)
(global-set-key (kbd "C-x b")   'helm-buffers-list)
(global-set-key (kbd "C-x s")   'save-buffer)

(global-set-key (kbd "C-x a") 'beginning-of-buffer)   ;; beg of buffer
(global-set-key (kbd "C-x e") 'end-of-buffer)         ;; end of buffer

;; windows
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "s-<return>") 'toggle-frame-maximized) ;;

;; minor buffer
(global-set-key (kbd "M-x") 'helm-M-x)


;; ****************************************************
;; 2. GTD: org-mode
;; ****************************************************
(global-set-key (kbd "C-\\") #'(lambda() (interactive) (org-agenda nil "a")))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-link-store)

;; ****************************************************
;; 3. Project && magit
;; ****************************************************
(defhydra hydra-project (:color blue :hint nil)
  "
^^^^^^############### PROJECT #################
^search^          ^cmd^          ^project^
^^^^^^-----------------------------------------
_g_: grep        _b_: shell      _v_: view all
_t_: replace     _e_: eshell     _s_: status
^ ^              _c_: compile    _f_: find project
^ ^              ^ ^             _d_: dired
^ ^              ^ ^             _k_: kill all
"
  ;("s" project-search)
  ("g" project-find-regexp)
  ("t" project-query-replace-regexp)

  ("b" project-shell)
  ("e" project-eshell)
  ("c" project-compile)

  ("v" magit-list-repositories)
  ("s" magit-status)
  ("d" project-dired)
  ("f" project-switch-project)
  ("k" project-kill-buffers))
(global-set-key (kbd "C-SPC") 'hydra-project/body)

;; file && buffer
(global-set-key (kbd "C-x C-f")  'project-find-file)
(global-set-key (kbd "C-x C-b")  'my/project-switch-to-buffer)


;; ****************************************************
;; others
;; ****************************************************
(global-set-key (kbd "C-z") 'nil); suspend-frame最小化在X下作用不大
(global-set-key (kbd "s-n") 'nil); ns-new-frame new-frame不受现在配置影响, 因此删除

(global-set-key (kbd "C-j") 'nil); 留给mode使用(mode开启时候可用), 其他时候不使用
(global-set-key (kbd "C-o") 'nil); 留给mode使用(mode开启时候可用), 其他时候不使用
(global-set-key (kbd "C-r") 'nil); 留给mode使用(mode开启时候可用), 其他时候不使用
(global-set-key (kbd "C-t") 'nil); 留给mode使用(mode开启时候可用), 其他时候不使用


(provide 'init-global-shortkey)
