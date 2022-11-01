;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; directories & files
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

(setq deft-recursive t
      deft-recursive-ignore-dir-regexp (concat "\\(?:" "\\." "\\|\\.\\." "\\)$"))

(rx (group (or (and (: ".")))) eol)

(setf text-regex (concat "\\(bak*\\)\\|\\(biak\\)"))

(setf text-regex  (rx (or (and "logseq" not-newline "bak") (and "b" "ik"))))

(rx (group-n "asdf"))

(string-match text-regex "asdfg/logseq/bak")

(setq org-agenda-archives-mode t)
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
          ("[Q]"  :foreground "grey"         :weight bold))))

(after! org-capture
  (setq org-capture-templates
        `(("t" "Todo" entry (file ,(concat org-directory "/inbox.org")) "* TODO %?\n  %i\n  %a")
          ("r" "Read" entry (file ,(concat org-directory "/inbox.org")) "* TODO %? :Read:\n  %i\n  %a"))))

(after! org-appear
  (setq org-appear-trigger 'always
        org-appear-autolinks t))

(use-package! org-transclusion
  :after org
  :init
  (map! :map global-map "<f12>" #'org-transclusion-add
        :leader :prefix "n"
        :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

(use-package! org-sticky-header
  :after org)

(use-package! valign
  :after org
  :config
  (add-hook 'org-mode-hook #'valign-mode))

(use-package! org-super-agenda
  :after org
  :config
  (setq org-super-agenda-header-map (make-sparse-keymap))
  ;;(setq org-super-agenda-header-map evil-org-agenda-mode-map)
  (setq org-super-agenda-groups
        '((:name "Current" :time-grid t :todo "TODAY")
          (:name "Important" :priority "A")
          (:todo ("NEXT" "TODO"))
          (:todo ("PEND" "WILL"))))
  (org-super-agenda-mode))

;; fix deft title in roam file
(defun my/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
            (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
          (deft-base-filename file))))

(advice-add 'deft-parse-title :override #'my/deft-parse-title)

(setq deft-strip-summary-regexp
    (concat "\\("
            "[\n\t]" ;; blank
            "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
            "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
            "\\)"))

(if (display-graphic-p)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset (font-spec :family "Sarasa Gothic SC" :size 18))))

;; (if (display-graphic-p)
;;     (dolist (charset '(kana han cjk-misc bopomofo))
;;       (set-fontset-font (frame-parameter nil 'font)
;;                         charset (font-spec :family "Hiragino Sans GB" :size 18))))
;; (let ((org-super-agenda-groups
;;        '((:auto-group t))))
;;   (org-agenda-list))

;; (let ((org-super-agenda-groups '((:todo "NEXT") (:todo ("PEND" "WILL")))))
;;   (org-todo-list))
