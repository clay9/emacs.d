;; ox-hugo
(require-package 'ox-hugo)
(require 'ox-hugo)

;; 默认不增加作者信息 (TODO 增加了作者信息之后, 输出的md文件, hugo server无法编译通过)
;; 因此暂时隐去作者信息
(setq org-export-with-author nil)

;; when export, do not raise an error on broken links -- 对publish无效
(setq org-export-with-broken-links t)

;; ********************
;; 自定义函数
;; ********************
;; function: ox-hugo (所有org文件) ==> md文件
(defun my-hugo-export-all (dir)
  (interactive)
  ;; 默认output-path为当前目录, 即hugo所在的目录
  (setq org-hugo-base-dir default-directory)

  ;; 递归获取dir下所有的org文件
  (dolist (file (directory-files-recursively dir ".org"))
    (with-current-buffer (find-file-noselect file)
      (org-hugo-export-to-md)
    )))


;; ********************
;; main
;; ********************
;; org-files 输出为 md-files
(my-hugo-export-all "./blog")
