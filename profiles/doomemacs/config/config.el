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
(setq doom-font (font-spec :family "Fira Code" :size 16))
;; (setq doom-font (font-spec :family "Source Code Pro" :size 18 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "Sarasa Gothic SC" :size 18)
;;       doom-unicode-font (font-spec :family "Sarasa Gothic SC" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-one)

(let ((guix-site-lisp-directory "~/.guix-profile/share/emacs/site-lisp"))
  (when (file-directory-p guix-site-lisp-directory)
    (let ((default-directory guix-site-lisp-directory))
     (normal-top-level-add-subdirs-to-load-path))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(display-time-mode)
(toggle-frame-maximized)

(after! minimap
   (setq minimap-minimum-width 10
         minimap-width-fraction 0.03))


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

(setq elfeed-db-directory "~/Sync/Database/Feeds")
(make-directory elfeed-db-directory t)

(when IS-WINDOWS
  (setq gc-cons-threshold (* 512 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 9 t #'garbage-collect))
  ;; (setq garbage-collection-messages t))

(after! eaf
  (setq eaf-config-location "~/Sync/Database/EAF"))

(after! mu4e
  (setq mu4e-root-maildir "~/Mails"
        mu4e-attachment-dir "~/Downloads"))
  ;; (setq mu4e-maildir "~/Emails")
  ;; (setq mu4e-get-mail-command "offlineimap -u quiet"
  ;;       mu4e-update-interval 60
  ;;       mu4e-view-show-images t))

(set-email-account! "qq"
  '((mu4e-sent-folder       . "/qq/Sent Mail")
    (mu4e-drafts-folder     . "/qq/Drafts")
    (mu4e-trash-folder      . "/qq/Trash")
    (mu4e-refile-folder     . "/qq/Inbox")
    (smtpmail-smtp-user     . "liszt21@qq.com")
    (user-mail-address      . "liszt21@qq.com")    ;; only needed for mu < 1.4
    (mu4e-compose-signature . "---\nYours truly\nThe Baz"))
  t)
(setq +mu4e-backend 'offlineimap)

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
        socks-server '("Default server" "127.0.0.1" 7890 5))
  (setenv "all_proxy" "http://127.0.0.1:7890")
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
