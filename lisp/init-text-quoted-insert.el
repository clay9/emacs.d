;;; init-text-quoted-insert.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; =====================
;;; quoted insert
;; =====================
(defun text/quoted-insert-l ()
  "Insert section break (^L) on its own line."
  (interactive)
  (unless (bolp)
    (end-of-line)
    (insert "\n"))
  (insert "\n\f\n"))

;; =====================
;;; forward-page, backward-page
;; =====================
(defun text/forward-page ()
  (interactive)
  (forward-page)
  (forward-line))

(defun text/backward-page ()
  (interactive)
  (backward-page)
  (forward-line -1))


(provide 'init-text-quoted-insert)
;;; init-text-quoted-insert.el ends here
