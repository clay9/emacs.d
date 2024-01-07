(require 'hydra)
(require 'gnus-topic)
(require 'gnus-sum)
(require 'gnus-art)
(require 'gnus-srvr)

;; ****************************************************
;; 0.server
;; ****************************************************
(defhydra hydra-gnus-server-mode (:color blue
					 :hint nil)
  ("o" gnus-server-open-server "open server" :color green)
  ("C-s" gnus-group-read-ephemeral-search-group "search" :color green))
(define-key gnus-server-mode-map (kbd "C-j") 'hydra-gnus-server-mode/body)


;; ****************************************************
;; 1.group
;; ****************************************************
(defhydra hydra-gnus-group-subscribe (:color blue
				:hint nil)
  "
^subscribe^          ^level^          ^kill^
^^^^^^----------------------------------------
_t_: toggle          _l_: set level    _k_: kill current group
_s_: propmt          ^ ^               _y_: yank killed group
^ ^                  ^ ^               _w_: kill region groups
^ ^                  ^ ^               _z_: kill zombile groups
^ ^                  ^ ^               _C-k_: kill groups on a certain level
"
  ("t" gnus-group-toggle-subscription-at-point)
  ("s" gnus-group-toggle-subscription)

  ("k" gnus-group-kill-group)
  ("y" gnus-group-yank-group)
  ("w" gnus-group-kill-region)
  ("z" gnus-group-kill-all-zombies)
  ("C-k" gnus-group-kill-level)

  ("l" gnus-group-set-current-level))
(defhydra hydra-gnus-topic (:color blue
				   :hint nil)
  "
^group^          ^topic create^          ^topic move^
^^^^^^------------------------------------------------------------------
_m_: move group  _n_: crate topic        _j_: jump to topic
_c_: copy group  _i_: indent topic       _s_: toggle hide|show empty topic
_D_: rm group    _u_: unindent topic     _C-k_: kill topic and all its groups
^ ^              _r_: rename topic       _C-y_: yank killed topic
"
  ("m" gnus-topic-move-group)
  ("c" gnus-topic-copy-group)
  ("D" gnus-topic-remove-group)

  ("n" gnus-topic-create-topic)
  ("i" gnus-topic-indent)
  ("u" gnus-topic-unindent)
  ("r" gnus-topic-rename)

  ("j" gnus-topic-jump-to-topic)
  ("s" gnus-topic-toggle-display-empty-topics)
  ("C-k" gnus-topic-kill-group)
  ("C-y" gnus-topic-yank-group))
(defhydra hydra-gnus-group-mode (:color blue
				:hint nil)
  "
^topic^          ^group^          ^mark^          ^post^          ^other^
^^^^^^--------------------------------------------------------------------------------
_t_: topic       _g_: group       _u_: read       _a_: post news  _r_: [r]efresh
^ ^              _j_: jump        _U_: unread     _m_: mail       _-_: server-mode
^ ^              _k_: j best      ^ ^             _i_: news       ^ ^
^ ^              _s_: [s] toggle  ^ ^             ^ ^             ^ ^
"
  ("t" hydra-gnus-topic/body)

  ("g" hydra-gnus-group-subscribe/body)
  ("j" gnus-group-jump-to-group)
  ("k" gnus-group-best-unread-group)
  ("s" my/gnus-toggle-unread)

  ("u" gnus-group-catchup-current)
;;  ("R" gnus-group-catchup-current-all)
  ("U" gnus-group-clear-data)

  ("a" gnus-group-post-news)
  ("m" gnus-group-mail)
  ("i" gnus-group-news)

  ("r" my/gnus-refresh)
  ("-" gnus-group-enter-server-mode)

  ("C-s" gnus-group-read-ephemeral-search-group "search" :color green))
(define-key gnus-group-mode-map (kbd "C-j") 'hydra-gnus-group-mode/body)

(defun my/gnus-topic-toggle-hide (parent)
  "Toggle hide/show topic"
  (let* ((topic (if parent (gnus-topic-parent-topic (gnus-current-topic)) (gnus-current-topic)))
	 (visible (cadr (cadr (gnus-topic-find-topology topic)))))
    (gnus-topic-goto-topic topic)
    (if (string= visible "visible")
	(gnus-topic-hide-topic)
      (gnus-topic-show-topic))))
(define-key gnus-group-mode-map (kbd "<tab>") #'(lambda() (interactive) (my/gnus-topic-toggle-hide nil)))
(define-key gnus-topic-mode-map (kbd "<tab>") #'(lambda() (interactive) (my/gnus-topic-toggle-hide nil)))
(define-key gnus-group-mode-map (kbd "<backtab>") #'(lambda() (interactive) (my/gnus-topic-toggle-hide t)))
(define-key gnus-topic-mode-map (kbd "<backtab>") #'(lambda() (interactive) (my/gnus-topic-toggle-hide t)))

(defun my/gnus-topic-move (previous)
  "Move topic up or down"
  (let* ((number (if previous -1 1))
	 (max-line (line-number-at-pos (point-max))))
    (gnus-topic-goto-topic (gnus-current-topic))

    ;; 如果自身是invisble, 就不要再去处理自身的children了
    (when (not (gnus-topic-visible-p))
      (if previous (gnus-topic-previous-topic 1) (gnus-topic-next-topic 1)))

    ;; 寻找下一个topic
    (forward-line number)
    (while (and (not (gnus-group-topic-p))
		(< (line-number-at-pos) max-line))
      (forward-line number))))
(define-key gnus-group-mode-map (kbd "C-p") #'(lambda() (interactive) (my/gnus-topic-move t)))
(define-key gnus-group-mode-map (kbd "C-n") #'(lambda() (interactive) (my/gnus-topic-move nil)))

(define-key gnus-group-mode-map (kbd "p") 'gnus-group-prev-group)
(define-key gnus-group-mode-map (kbd "n") 'gnus-group-next-group)

;; toggle show unread group && unread topic
(setq my/gnus-list-all-groups nil)
(defun my/gnus-toggle-unread()
  (interactive)
  (setq my/gnus-list-all-groups (not my/gnus-list-all-groups))
  (setq gnus-topic-display-empty-topics my/gnus-list-all-groups)
  (if my/gnus-list-all-groups
      (gnus-group-list-all-groups)
    (gnus-group-list-groups (or (gnus-group-default-level nil t)
				(gnus-group-default-list-level)
				gnus-level-subscribed)
			    nil nil t)))
(define-key gnus-group-mode-map (kbd "s") 'my/gnus-toggle-unread)

;; refresh
(defun my/gnus-refresh()
  (interactive)
  (gnus-group-get-new-news) ;;get new news
  (gnus-group-check-bogus-groups t);; find && delete bogus groups
  (gnus-group-find-new-groups) ;; find new groups
  ;; topic match
  (gnus-topic-move-matching "nnimap.*" "archive" nil)
  (gnus-topic-move-matching "nnimap.*INBOX" "mail" nil)
  (gnus-group-save-newsrc))
(define-key gnus-group-mode-map (kbd "r") 'my/gnus-refresh)


;; ****************************************************
;; 2.summary
;; ****************************************************
(defhydra hydra-gnus-summary-limit (:color blue
				:hint nil)
  "
^limit^
^^^^^^------------------------------------------------------------------------------
_s_: limit to subject
_a_: limit to author
_R_: limit to recipient
_A_: limit to address
_S_: limit to singletones
_x_: limit to extra
_u_: limit to unread
_m_: limit to marks
_t_: limit to age
_n_: limit to articles
_w_: pop limit
_._: limit to unseen
_v_: limit to score
_P_: limit to display predicate
_r_: limit to replied
_E_: limit include expunged
_D_: limit include dormant
"
  ("s"  gnus-summary-limit-to-subject)
  ("a"  gnus-summary-limit-to-author)
  ("R"  gnus-summary-limit-to-recipient)
  ("A"  gnus-summary-limit-to-address)
  ("S"  gnus-summary-limit-to-singletones)
  ("x"  gnus-summary-limit-to-extra)
  ("u"  gnus-summary-limit-to-unread)
  ("m"  gnus-summary-limit-to-marks)
  ("t"  gnus-summary-limit-to-age)
  ("n"  gnus-summary-limit-to-articles)
  ("w"  gnus-summary-pop-limit)
  ("."  gnus-summary-limit-to-unseen)
  ("v"  gnus-summary-limit-to-score)
  ("P"  gnus-summary-limit-to-display-predicate)
  ("r"  gnus-summary-limit-to-replied)
  ("E"  gnus-summary-limit-include-expunged)
  ("D"  gnus-summary-limit-include-dormant))

(defhydra hydra-gnus-summary-mode (:color blue
				:hint nil)
  "
^select^          ^followup^          ^mark^          ^limit^
^^^^^^-----------------------------------------------------------
_j_: goto         _f_: followup       _!_: ticked     _/_: limit
_l_: last         _F_: with origin    _u_: unread     ^ ^
^ ^               ^ ^                 ^ ^             ^ ^
_h_: switch       _r_: mail           _=_: cache      ^ ^
^ ^               _R_: with origin    _-_: uncache    ^ ^
^ ^               _c_: cc	      ^ ^
"
  ("j" gnus-summary-goto-subject)
  ("l" gnus-summary-goto-last-article)
  ("h" gnus-summary-select-article-buffer)

  ("f" gnus-summary-followup)
  ("F" gnus-summary-followup-with-original)

  ("r" gnus-summary-reply)
  ("R" gnus-summary-reply-with-original)
  ("c"  gnus-summary-mail-forward)

  ("a" gnus-summary-post-news)
  ("C-c" gnus-summary-supersede-article)

  ("!" gnus-summary-tick-article-forward)
  ("u" #'(lambda () (interactive) (gnus-summary-put-mark-as-unread 1)))
  ("=" gnus-cache-enter-articles)
  ("-" gnus-cache-remove-articles)

  ("/" hydra-gnus-summary-limit/body))
(define-key gnus-summary-mode-map (kbd "C-j") 'hydra-gnus-summary-mode/body)

(define-key gnus-summary-mode-map (kbd "p") 'gnus-summary-prev-unread-subject)
(define-key gnus-summary-mode-map (kbd "n") 'gnus-summary-next-unread-subject)

;; when move in article-buff, dont goto prev|next article
(defun my/gnus-summary-enter-article(enter-article)
  (gnus-summary-next-page nil nil t)
  (when enter-article
    (gnus-summary-select-article-buffer)))
(define-key gnus-summary-mode-map (kbd "SPC") #'(lambda() (interactive) (my/gnus-summary-enter-article nil)))
(define-key gnus-summary-mode-map (kbd "<return>") #'(lambda() (interactive) (my/gnus-summary-enter-article t)))

(defun my/gnus-summary-quit ()
  "when article-window exist, delete article-window. otherwise quit gnus summary"
  (interactive)
  (let* ((article-buff (get-buffer "*Article*"))
	 (article-win (get-buffer-window article-buff)))
    (if (and article-buff
	     (window-live-p article-win))
	(delete-window article-win)
      (gnus-summary-exit nil nil))))
(define-key gnus-summary-mode-map (kbd "q") 'my/gnus-summary-quit)


;; ****************************************************
;; 3.article
;; ****************************************************
(defhydra hydra-gnus-article-mode (:color blue
				:hint nil)
  "
^select^          ^followup^          ^mark^          ^attach^
^^^^^^---------------------------------------------------------
^ ^               _f_: followup       _!_: ticked     _s_: save
^ ^               _F_: with origin    _u_: unread     _o_: open
^ ^               ^ ^                 ^ ^             ^ ^
_h_: switch       _r_: mail           _=_: cache      ^ ^
^ ^               _R_: with origin    _-_: uncache    ^ ^
^ ^               _c_: cc             ^ ^             ^ ^
"
  ("h" gnus-article-show-summary)

  ("f" gnus-summary-followup)
  ("F" gnus-summary-followup-with-original)

  ("r" gnus-summary-reply)
  ("R" gnus-summary-reply-with-original)
  ("c"  gnus-summary-mail-forward)

  ("a" gnus-summary-post-news)
  ("C-c" gnus-summary-supersede-article)

  ("!" #'(lambda() (interactive)
	   (gnus-article-show-summary)
	   (gnus-summary-tick-article-forward 1)
	   (gnus-summary-select-article-buffer)))
  ("u" #'(lambda() (interactive)
	   (gnus-article-show-summary)
	   (gnus-summary-put-mark-as-unread 1)
	   (gnus-summary-select-article-buffer)))
  ("=" #'(lambda() (interactive)
	   (gnus-article-show-summary)
	   (gnus-cache-enter-articles)
	   (gnus-summary-select-article-buffer)))
  ("-" #'(lambda() (interactive)
	   (gnus-article-show-summary)
	   (gnus-cache-remove-articles)
	   (gnus-summary-select-article-buffer)))

  ("s" gnus-mime-save-part)
  ("o" gnus-article-press-button))
(define-key gnus-article-mode-map (kbd "C-j") 'hydra-gnus-article-mode/body)
(define-key gnus-article-mode-map (kbd "q") 'delete-window) ;;delete article window, not quit summary-buffer

;; when move in article-buff, dont goto prev|next article
(define-key gnus-article-mode-map (kbd "SPC") #'(lambda () (interactive)
						  (gnus-article-next-page)))
(define-key gnus-article-mode-map (kbd "DEL") #'(lambda () (interactive)
						  (gnus-article-prev-page)))


;; ****************************************************
;; 4. message
;; ****************************************************
(defhydra hydra-message-mode (:color blue
				     :hint nil)
  "
_c_: complete mail address
_a_: attach file
"
  ("c" nil)
  ("a" mml-attach-file))
(define-key message-mode-map (kbd "C-j") 'hydra-message-mode/body)


(provide 'init-gnus-shortkey)
