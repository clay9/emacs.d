;;; init-text-navigation.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; =====================
;;; outline
;; =====================
(transient-define-prefix transient/navigation-outline ()
  [ "    -------- Outline --------"
    [:class transient-column "Search"
            ("s" "search" consult-outline)]
    [:class transient-column "Toggle"
            ("TAB" "toggle" text/outline-cycle :transient t)
            ("a" "toggle all" text/outline-toggle-all :transient t)
            ("h" "hide other" outline-hide-other :transient t)]
    [:class transient-column "Navigation"
            ("p" "previous" outline-previous-visible-heading :transient t)
            ("n" "next" outline-next-visible-heading :transient t)
            ("C-p" "previous same" outline-backward-same-level :transient t)
            ("C-n" "next same" outline-forward-same-level :transient t)
            ("t" "up" outline-up-heading :transient t)]])

;; =====================
;;; 
;; =====================
(transient-define-prefix transient/navigation-quoted-l ()
  [ "---- ^L ----"
    [:class transient-column "Navigation"
            ("p" "previous" text/forward-page :transient t)
            ("n" "next" text/backward-page :transient t)]])

;; ----------------------
;;; Keybinding
;; ----------------------
(defun text/navigation ()
  "Context-aware for navigation."
  (interactive)
  (cond ((bound-and-true-p org-mode)
         (consult-org-heading))
        ((bound-and-true-p outline-minor-mode)
         (transient/navigation-outline))
        (t
         (transient/navigation-quoted-l))))

(provide 'init-text-navigation)
;;; init-text-navigation.el ends here
