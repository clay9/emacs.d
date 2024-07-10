;;; init-outline.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package symbols-outline
  ;; :commands (symbols-outline-show)
  :bind (:map symbols-outline-mode-map
         ("TAB" . my/symbols-outline-tab)
         ("<backtab>" . my/symbols-outline-shifttab))
  :config
  (setq symbols-outline-window-position 'left
        symbols-outline-no-other-window nil
        symbols-outline-fetch-fn #'symbols-outline-lsp-fetch)
  (symbols-outline-follow-mode))

;; TODO not work in use-package
(require 'symbols-outline)
(defun my/symbols-outline-tab()
    (interactive)
    "cycle through visibility levels like `org-mode' shift-tab."
    (let* ((tree (get-text-property (line-beginning-position) 'node))
           (deepest-depth (symbols-outline-node-depth-non-collpased tree))
           (symbols-outline--refreshing nil)
           (inhibit-read-only t)
           depth-target)
      (cond ((= deepest-depth 1) ;; Expand all nodes
             (symbols-outline-node-foreach
              tree
              (lambda (node) (setf (symbols-outline-node-collapsed node) nil))))
            ((= deepest-depth 2) ;; close node
             (symbols-outline-node-foreach-at-depth
              tree 0
              (lambda (node) (setf (symbols-outline-node-collapsed node) t))))
            ((> deepest-depth 2) ;; Hide nodes of deepest depth
             (setq depth-target (- deepest-depth 2))
             (when (> depth-target 2) (setq depth-target 2))
             (symbols-outline-node-foreach-at-depth
              tree depth-target
              (lambda (node) (setf (symbols-outline-node-collapsed node) t)))))
      (symbols-outline--render)))
(defun my/symbols-outline-shifttab()
    (interactive)
    "global cycle through visibility levels like `org-mode' shift-tab."
    (let* ((tree (with-current-buffer symbols-outline--origin
                   symbols-outline--entries-tree))
           (deepest-depth (symbols-outline-node-depth-non-collpased tree))
           (symbols-outline--refreshing nil)
           (inhibit-read-only t))
      (message "deep: %s" deepest-depth)
      (cond ((= deepest-depth 2) ;; Expand all nodes
             (symbols-outline-node-foreach
              tree
              (lambda (node) (setf (symbols-outline-node-collapsed node) nil))))
            ((= deepest-depth 3) ;; close node
             (symbols-outline-node-foreach-at-depth
              tree 1
              (lambda (node) (setf (symbols-outline-node-collapsed node) t))))
            ((> deepest-depth 3) ;; Hide nodes of deepest depth
             (symbols-outline-node-foreach-at-depth
              tree 2
              (lambda (node) (setf (symbols-outline-node-collapsed node) t)))))
      (symbols-outline--render)))

(setq symbols-outline-nerd-icon-alist
    '(("tag"           . "⌄")

      ;; C, C++, java, python
      ("file"          . "📁")
      ("function"      . "⨍")
      ("method"        . "⧫")
      ("prototype"     . "1")
      ("annotation"    . "2")
      ("constructor"   . "3")
      ("class"         . "Ⓒ")
      ("struct"        . "Ⓣ")
      ("interface"     . "4")
      ("union"         . "⌄")
      ("enum"          . "6")
      ("enumerator"    . "7")
      ("enummember"    . "8")
      ("using"         . "9")
      ("namespace"     . "ღ")
      ("variable"      . "10")
      ("member"        . "11")
      ("field"         . "12")
      ("externvar"     . "13")
      ("local"         . "14")
      ("macro"         . "15")
      ("string"        . "16")
      ("boolean"       . "17")
      ("array"         . "18")
      ("number"        . "19")
      ("object"        . "20")
      ("misc"          . "21")
      ("operator"      . "22")
      ("parameter"     . "23")
      ("macroparam"    . "24")
      ("typeparameter" . "25")
      ("tparam"        . "26")
      ("event"         . "27")
      ("typedef"       . "28")
      ("package"       . "29")
      ("module"        . "30")
      ("key"           . "31")
      ("null"          . "32")
      ("keyword"       . "33")

      ;; Elisp
      ("derivedMode"   . "34")
      ("majorMode"     . "35")
      ("minorMode"     . "36")
      ("inline"        . "37")
      ("subst"         . "38")
      ("group"         . "39")
      ("error"         . "40")
      ("custom"        . "41")
      ("face"          . "42")
      ("const"         . "43")
      ("alias"         . "44")
      ("unknown"       . "45")

      ;; JavaScript, TypeScript
      ("constant"      . "46")
      ("property"      . "47")

      ;; Markdown
      ("chapter"       . "i")
      ("section"       . "h")
      ("subsection"    . "g")
      ("subsubsection" . "f")
      ("l4subsection"  . "e")
      ("l5subsection"  . "d")

      ;; Org
      ("part"          . "c")

      ;; Chevrons
      ("chevron-down"  . "")
      ("chevron-right" . "⋮")
      ))

(provide 'init-outline)
;;; init-outline.el ends here
