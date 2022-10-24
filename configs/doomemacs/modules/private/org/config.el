;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; directories & files
(setq org-directory  "~/Notes"
      org-archive-location (concat org-directory "/archive/%s::")
      org-agenda-files (list org-directory)
      deft-directory org-directory
      org-roam-directory org-directory
      org-brain-path org-directory
      org-roam-file-exclude-regexp ".*/bak/.*"
      org-brain-visualize-default-choices 'all
      rmh-elfeed-org-files (list (concat org-directory "/feeds.org")))
      ;; org-roam-tag-sources '(prop last-directory)
      ;; org-brain-include-file-entries t
      ;; org-brain-file-entries-use-title t
      

(setq org-export-select-tags '("Publish" "Public" "export")
      org-publish-project-alist
      '(("content"
         :base-directory "~/Notes/content"
         :publishing-directory "~/Notes/publish"
         ;; :publishing-function (org-org-publish-to-org org-md-publish-to-md)
         :publishing-function org-org-publish-to-org
         :select-tags ("Publish" "Public" "export" "Export" "publish" "public")
         :exclude-tags ("Private" "Secret" "noexport")
         :recursive t
         :with-broken-links t
         :with-toc nil)))

;; ui
(after! org
  ;; (setq org-log-done 'time)
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
          ("[Q]"  :foreground "grey"         :weight bold))))

(after! org-capture
  (setq org-capture-templates
        `(("t" "Todo" entry (file ,(concat org-directory "/Inbox.org")) "* TODO %?\n  %i\n  %a")
          ("r" "Read" entry (file ,(concat org-directory "/Inbox.org")) "* TODO %? :Read:\n  %i\n  %a"))))

(use-package! org-transclusion
  :after org
  :init
  (map! :map global-map "<f12>" #'org-transclusion-add
        :leader :prefix "n"
        :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))
