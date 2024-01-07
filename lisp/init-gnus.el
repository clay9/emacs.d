(require 'gnus)

;; who are you
(setq user-mail-address	 "x@gmail.com"
      user-full-name	 "clay9" )

;; ****************************************************
;; startup
;; ****************************************************
; turn off read|write the .newsrc
(setq gnus-save-newsrc-file nil)
(setq gnus-read-newsrc-file nil)

(setq gnus-init-file (concat my/ecfg-dir "gnus/gnus.el"))
(setq gnus-startup-file (concat my/ecfg-dir "gnus/newsrc"))

;; turn off backup
(defun turn-off-backup ()
  (set (make-local-variable 'backup-inhibited) t))
(add-hook 'gnus-save-quick-newsrc-hook 'turn-off-backup)
(add-hook 'gnus-save-standard-newsrc-hook 'turn-off-backup)

;; auto read dribble file
(setq gnus-always-read-dribble-file t)

;; window layout
(setq gnus-always-force-window-configuration t)
(setq gnus-use-full-window nil)


;; ****************************************************
;; server
;; ****************************************************
(setq gnus-select-method '(nntp "news.gnus.org"))

(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "gmail"
		      (nnimap-address "imap.gmail.com")
		      (nnimap-stream ssl)
		      (nnimap-server-port 993)
		      (nnimap-inbox "INBOX")
		      (nnir-search-engine imap)))
(add-to-list 'gnus-secondary-select-methods
	     '(nnimap "qq"
		      (nnimap-address "imap.qq.com")
		      (nnimap-stream ssl)
		      (nnimap-server-port 993)
		      (nnimap-inbox "INBOX")
		      (nnir-search-engine imap)))


;; ****************************************************
;; group
;; ****************************************************
;; topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

; Don't get the first article automatically:
(setq gnus-auto-select-first nil)
;; control empty topic display
(setq gnus-topic-display-empty-topics nil)


;; ****************************************************
;; mail -- 特殊group
;; ****************************************************
;; splitting mails
(setq nnimap-split-methods
      '(("apple" "^From:.*apple.com")
	("github" "^From:.*noreply@github.com")
	("github_notice" "^From:.*notifications@github.com")
	("steam" "^From:.*steampowered.com")
	("bank" "^From:.*message.cmbchina.com")
	("google" "^From:.*no-reply@accounts.google.com")
	("emacs" "^Subject:.*emacs")
	("errors" "^From:.*\\(mailer.daemon\\|postmaster\\)")))

;; post styles
(setq message-send-mail-function 'smtpmail-send-it)
(setq gnus-posting-styles
      '((".*" ; Matches all groups of messages
	 (name "clay9")
	 (address "wcq377133665@gmail.com")
	 ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587"))
	("^gnus" ; Matches Gnus group called "work"
	 (name "wcq")
	 (address "377133665@qq.com")
	 ("X-Message-SMTP-Method" "smtp smtp.qq.com 587"))))


;; ****************************************************
;; summary
;; ****************************************************
;; when summary exit, dont jump next group
(setq gnus-summary-next-group-on-exit nil)

;;suummary format
(setq gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
      gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
      gnus-thread-sort-functions '(gnus-thread-sort-by-date)
      gnus-sum-thread-tree-false-root ""
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├► "
      gnus-sum-thread-tree-root ""
      gnus-sum-thread-tree-single-leaf "╰► "
      gnus-sum-thread-tree-vertical "│")

;; (gnus-add-configuration
;;  '(article (vertical 1.0
;;	       (group 4)
;;	       (summary .25 point)
;;	       (article 1.0))))


;; ****************************************************
;; article
;; ****************************************************
(setq gnus-single-article-buffer t)
(setq gnus-article-over-scroll t)

(provide 'init-gnus)
