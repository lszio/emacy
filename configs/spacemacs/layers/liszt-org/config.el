;; liszt-org config
(setq org-directory "~/Notes"
      org-archive-location (concat org-directory "/Archive/%s::")
      org-refile-targets '((nil . (:tag . "Done")) (nil . (:level . 1)))
      org-roam-directory org-directory
      org-roam-v2-ack t
      org-roam-file-exclude-regexp ".*/bak/.*"
      deft-directory org-directory
      deft-extensions '("md" "org")
      deft-recursive t
      org-log-done 'time
      org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))

(setq org-todo-keywords
        '((sequence "NEXT(n)"
                    "TODO(t)"
                    "PEND(p)"
                    "WILL(w@/!)"
                    "|"
                    "DONE(d)"
                    "QUIT(q@)")
          (sequence "[-](N)"
                    "[ ](T)"
                    "[:](P)"
                    "[?](W)"
                    "|"
                    "[X](D)"
                    "[Q](Q)")))

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

(setq org-capture-templates
      `(("t" "Todo"
         entry (file ,(concat org-directory "/Inbox.org"))
         "* TODO %?\n  %i\n  %a")
        ("r" "Read"
         entry (file ,(concat org-directory "/Inbox.org"))
         "* TODO %? :Read:\n  %i\n  %a")))

;; fix deft title in roam file
(defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
            (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
          (deft-base-filename file))))

(advice-add 'deft-parse-title :override #'cm/deft-parse-title)

(setq deft-strip-summary-regexp
    (concat "\\("
            "[\n\t]" ;; blank
            "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
            "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
            "\\)"))
