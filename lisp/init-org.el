;;; init-org.el --- org -*- lexical-binding: t -*-

;; Author: Li Shuzhi
;; Maintainer: Li Shuzhi

;;; Commentary:

;;; Code:
(use-package org-roam
  :after org
  :init
  (setq org-roam-directory org-directory
        org-roam-v2-ack t)
  :config
  (org-roam-setup)
  (add-to-list 'display-buffer-alist
               '(("*org-roam*"
                  (display-buffer-in-direcion)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
  :general
  (tyrant-def
    "n" (cons "Notes" (make-sparse-keymap))
    "n b" 'org-roam-buffer-toggle
    "n f" 'org-roam-node-find
    "n g" 'org-roam-graph
    "n i" 'org-roam-node-insert
    "n c" 'org-roam-capture
    "n t" 'org-roam-tag-add
    "n r" 'org-roam-ref-add
    "n a" 'org-roam-alias-add))

(use-package org-roam-ui)

(setq org-directory  "~/Notes"
      org-archive-location (concat org-directory "/archive/%s::")
      org-contacts-files (list (concat org-directory "/archive/contacts.org"))
      org-agenda-files (list org-directory)
      deft-directory org-directory
      org-roam-directory org-directory
      org-brain-path org-directory
      org-roam-file-exclude-regexp ".*/bak/.*"
      org-brain-visualize-default-choices 'all
      rmh-elfeed-org-files (list (concat org-directory "/feeds.org")))

(setq org-fontify-whole-heading-line t
      org-fontify-quote-and-verse-blocks t
      org-hide-macro-markers t
      org-startup-indented t
      org-adapt-indentation t
      org-startup-with-inline-images t
      org-startup-with-latex-preview t)

(setq deft-recursive t
      deft-recursive-ignore-dir-regexp (rx (or "." ".." "logseq") eol))

(setq org-agenda-archives-mode t)
(setq org-export-select-tags '("Publish" "Public" "export")
      org-publish-project-alist
      '(("content"
         :base-directory "~/Notes/content"
         :publishing-directory "~/Notes/publish"
         ;; :publishing-function (org-org-publish-to-org org-md-publish-to-md)
         :publishing-function org-org-publish-to-org
         :select-tags ("Publish" "Public" "Export" "export" "publish" "public")
         :exclude-tags ("Private" "Secret" "noexport")
         :recursive t
         :with-broken-links t
         :with-toc nil)))

(setq org-log-done 'time)
;; (setq org-priority-lowest ?E)
(setq org-contacts-icon-use-gravatar nil)
(setq org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
(setq org-todo-keywords
      '((sequence "NEXT(n)" "TODO(t)" "PEND(p)" "WILL(w@/!)" "|" "DONE(d)" "QUIT(q@)")
        (sequence "[-](N)" "[ ](T)" "[:](P)" "[?](W)" "|" "[X](D)" "[Q](Q)")))
(setq org-todo-keyword-faces
      '(("TODO" :foreground "orange"       :weight bold)
        ("[ ]"  :foreground "orange"       :weight bold)
        ("NEXT" :foreground "yellow"       :weight bold)
        ("[-]"  :foreground "yellow"       :weight bold)
        ("PEND" :foreground "pink"         :weight bold)
        ("[:]"  :foreground "pink"         :weight bold)
        ("WILL" :foreground "purple"       :weight bold)
        ("[?]"  :foreground "purple"       :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("[X]"  :foreground "forest green" :weight bold)
        ("QUIT" :foreground "grey"         :weight bold)
        ("[Q]"  :foreground "grey"         :weight bold)))

(use-package org-contrib)

(use-package org-modern
  :ensure t
  :hook (elpaca-after-init . (lambda () (setq org-modern-hide-stars 'leading) (global-org-modern-mode t)))
  :config
  ;; 标题行型号字符
  (setq org-modern-star ["☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"])
  ;; 额外的行间距，0.1表示10%，1表示1px
  (setq-default line-spacing 0.1)
  ;; tag边框宽度，还可以设置为 `auto' 即自动计算
  (setq org-modern-label-border 1)
  ;; 设置表格竖线宽度，默认为3
  (setq org-modern-table-vertical 2)
  ;; 设置表格横线为0，默认为0.1
  (setq org-modern-table-horizontal 0)
  ;; 复选框美化
  (setq org-modern-checkbox
        '((?X . #("▢✓" 0 2 (composition ((2)))))
          (?- . #("▢–" 0 2 (composition ((2)))))
          (?\s . #("▢" 0 1 (composition ((1)))))))
  ;; 列表符号美化
  (setq org-modern-list
        '((?- . "•")
          (?+ . "◦")
          (?* . "▹")))
  ;; 代码块左边加上一条竖边线（需要Org mode顶头，如果启用了 `visual-fill-column-mode' 会很难看）
  (setq org-modern-block-fringe t)
  ;; 代码块类型美化，我们使用了 `prettify-symbols-mode'
  (setq org-modern-block-name nil)
  ;; #+关键字美化，我们使用了 `prettify-symbols-mode'
  (setq org-modern-keyword nil))

(use-package org-appear
 :ensure t
 :hook (org-mode . org-appear-mode)
 :config
 (setq org-appear-autolinks t)
 (setq org-appear-autosubmarkers t)
 (setq org-appear-autoentities t)
 (setq org-appear-autokeywords t)
 (setq org-appear-inside-latex t))

(use-package elfeed)

(use-package elfeed-org
  :config
  (setq rmh-elfeed-org-files (list "~/Notes/feeds.org"))
  (elfeed-org))

(use-package org-fragtog :hook (org-mode . org-fragtog-mode))

;; (use-package tex :elpaca auctex)

(use-package mpvi :elpaca (mpvi :repo "https://github.com/lorniu/mpvi")
  :general
  (tyrant-def))

(provide 'init-org)

;;; init-org.el ends here
