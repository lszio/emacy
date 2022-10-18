;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;;
(setq user-full-name "Liszt21"
      user-mail-address "1832666492@qq.com")

(setq liszt-home "~")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 18 :weight 'semi-light)
;;     doom-variable-pitch-font (font-spec :family "sans" :size 20))
(setq doom-font (font-spec :family "Fira Code" :size 18 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Sarasa Gothic SC" :size 18)
      doom-unicode-font (font-spec :family "Sarasa Gothic SC" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(display-time-mode)
(toggle-frame-maximized)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (concat liszt-home "/Notes")
      org-archive-location (concat org-directory "/Archive/%s::")
      deft-directory org-directory
      ;; org-roam
      org-roam-directory org-directory
      org-roam-file-exclude-regexp ".*/bak/.*"
      ;; org-roam-tag-sources '(prop last-directory)
      ;; org-brain
      org-brain-path org-directory
      org-brain-visualize-default-choices 'all
      ;; org-brain-include-file-entries t
      ;; org-brain-file-entries-use-title t
      org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))

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

(setq rmh-elfeed-org-files (list (concat org-directory "/feeds.org")))

(after! org
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
  (setq org-log-done 'time)
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
        `(("t" "Todo"
           entry (file ,(concat org-directory "/Inbox.org"))
           "* TODO %?\n  %i\n  %a")
          ("r" "Read"
           entry (file ,(concat org-directory "/Inbox.org"))
           "* TODO %? :Read:\n  %i\n  %a"))))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; Key-bindings

(after! sly
  (setq inferior-lisp-program "ros")
  (setq sly-lisp-implementations
        '((ros ("ros" "run"))
          (sbcl ("ros" "-L" "sbcl-bin" "run"))
          (ccl ("ros" "-L" "ccl-bin" "run")))))

(use-package rime
  :defer t
  :custom
  (rime-user-data-dir (if IS-LINUX (if IS-WSL "/mnt/c/Users/Liszt/AppData/Roaming/Rime" "~/.config/fcitx/rime") "C:/Users/Liszt/AppData/Roaming/Rime"))
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates '(rime-predicate-after-alphabet-char-p ;; 在英文字符串之后（必须为以字母开头的英文字符串）
                             ;; rime-predicate-after-ascii-char-p ;; 任意英文字符后
                             rime-predicate-prog-in-code-p ;; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
                             rime-predicate-in-code-string-p ;; 在代码的字符串中，不含注释的字符串。
                             rime-predicate-evil-mode-p ;; 在 evil-mode 的非编辑状态下
                             ;; rime-predicate-ace-window-p ;; 激活 ace-window-mode
                             ;; rime-predicate-hydra-p ;; 如果激活了一个 hydra keymap
                             ;; rime-predicate-current-input-punctuation-p ;; 当要输入的是符号时
                             rime-predicate-punctuation-after-space-cc-p ;; 当要在中文字符且有空格之后输入符号时
                             rime-predicate-punctuation-after-ascii-p ;; 当要在任意英文字符之后输入符号时
                             rime-predicate-punctuation-line-begin-p ;; 在行首要输入符号时
                             rime-predicate-space-after-ascii-p ;; 在任意英文字符且有空格之后
                             rime-predicate-space-after-cc-p ;; 在中文字符且有空格之后
                             rime-predicate-current-uppercase-letter-p ;; 将要输入的为大写字母时
                             rime-predicate-tex-math-or-command-p));; 在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
  :bind
  (:map rime-mode-map
   ("C-," . 'rime-force-enable)))

(when IS-WINDOWS
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect) 
  (setq garbage-collection-messages t))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package! eaf
  :config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-music-player)
  (require 'eaf-video-player)
  (require 'eaf-image-viewer)
  (require 'eaf-rss-reader)
  (require 'eaf-terminal)
  (require 'eaf-markdown-previewer)
  (require 'eaf-org-previewer)
  (require 'eaf-camera)
  (require 'eaf-git)
  (require 'eaf-file-manager)
  (require 'eaf-mindmap)
  (require 'eaf-netease-cloud-music)
  (require 'eaf-system-monitor)
  (require 'eaf-file-browser)
  (require 'eaf-file-sender)
  (require 'eaf-airshare)
  (require 'eaf-jupyter))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (when (fboundp 'cadddr)
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is %s:%d"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy"))))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost")
        socks-server '("Default server" "127.0.0.1" 1080 5))
  (setenv "all_proxy" "http://127.0.0.1:1080")
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'native
        socks-noproxy nil)
  (setenv "all_proxy" "")
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (require 'socks)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))
