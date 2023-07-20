
(defun fonts-installed (&rest font-list) (reverse (cl-intersection font-list (font-family-list) :test #'equal)))

(use-package fontaine
  :ensure t
  :when (display-graphic-p)
  :config
  (setq fontaine-latest-state-file (locate-user-emacs-file "etc/fontaine-latest-state.eld"))
  (setq fontaine-presets
    '((regular
       :default-height 140
       :default-weight regular
       :fixed-pitch-height 1.0
       :variable-pitch-height 1.0)
      (large
       :default-height 180
       :default-weight normal
       :fixed-pitch-height 1.0
       :variable-pitch-height 1.05)
      (t
       :default-family "Fira Code"
       :fixed-pitch-family "Fira Code"
       :variable-pitch-family "Fira Code"
       :italic-family "Fira Code"
       :variable-pitch-weight normal
       :bold-weight normal
       :italic-slant italic
       :line-spacing 0.1)))
  ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-set-preset 'regular)

  ;; set emoji font
  (set-fontset-font t (if (version< emacs-version "28.1") '(#x1f300 . #x1fad0) 'emoji)
    (car (fonts-installed
          "Noto Emoji"
          "Symbola"
          "Apple Color Emoji"
          "Noto Color Emoji"
          "Segoe UI Emoji")))
  ;; set Chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :family
                (car (fonts-installed
                       "LXGW Wenkai"
                       "霞鹜文楷"
                       "Sarasa Gothic SC"
                       "更纱黑体 SC")))))

  ;; set Chinese font scale
  (setq face-font-rescale-alist `(
                                  ("Symbola"             . 1.3)
                                  ("Microsoft YaHei"     . 1.2)
                                  ("WenQuanYi Zen Hei"   . 1.2)
                                  ("Sarasa Mono SC Nerd" . 1.2)
                                  ("PingFang SC"         . 1.16)
                                  ("Lantinghei SC"       . 1.16)
                                  ("Kaiti SC"            . 1.16)
                                  ("Yuanti SC"           . 1.16)
                                  ("Apple Color Emoji"   . 0.91))))
;; TODO
(use-package fontify-face)

(use-package doom-modeline
  :ensure t
  :hook (elpaca-after-init . doom-modeline-mode)
  :custom
  (doom-modeline-irc nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-gnus nil)
  (doom-modeline-github nil)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root) ; : auto
  (doom-modeline-persp-name nil)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count nil))

;; [[https://github.com/tarsius/minions][minions]] 插件能让模式栏变得清爽，将次要模式隐藏起来。
(use-package minions
  :ensure t
  :hook (elpaca-after-init . minions-mode))

(use-package keycast
  :ensure t
  :hook (elpaca-after-init . keycast-mode)
  ;; :custom-face
  ;; (keycast-key ((t (:background "#0030b4" :weight bold))))
  ;; (keycast-command ((t (:foreground "#0030b4" :weight bold))))
  :config
  ;; set for doom-modeline support
  ;; With the latest change 72d9add, mode-line-keycast needs to be modified to keycast-mode-line.
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (fix for use with doom-mode-line)."
    :global t
    (if keycast-mode)
    (progn))
  (add-hook 'pre-command-hook 'keycast--update t)
  (add-to-list 'global-mode-string '("" keycast-mode-line "  "))
  (remove-hook 'pre-command-hook 'keycast--update)
  (setq global-mode-string (delete '("" keycast-mode-line "  ") global-mode-string))

  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p mouse-movement-p mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil)))

  (setq keycast-log-format "%-20K%C\n")
  (setq keycast-log-frame-alist '((minibuffer . nil)))
  (setq keycast-log-newest-first t))

(use-package anzu
    :config
    (global-anzu-mode +1))

(use-package evil-anzu :after evil)

(require 'init-theme)

(provide 'init-ui)