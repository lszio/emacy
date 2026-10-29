;;; base.el --- editor configuration -*- lexical-binding: t; -*-
(require 'cl-lib)

(use-package no-littering
  :demand
  :config
  (setq custom-file (no-littering-expand-var-file-name "custom.el")
        whisper-install-directory (no-littering-expand-var-file-name "whisper"))
  (let ((dir (no-littering-expand-var-file-name "locks")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t)))))

(use-package which-key
  :demand t
  :init
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)
  (which-key-idle-delay 0.12)
  :diminish which-key-mode)

(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
  NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name :ensure nil ,@args))

(use-package helpful
  ;;:after evil
  :init
  (setq evil-lookup-func #'helpful-at-point)
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable))

(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(modify-coding-system-alist 'file "\\.org\\'" 'utf-8)
(when (display-graphic-p) (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(when windows? (set-w32-system-coding-system 'utf-8))

(use-package gcmh :demand t :config (gcmh-mode 1))

(use-package general
  :demand t
  :ensure (:wait t)
  :config
  (general-override-mode)
  (general-auto-unbind-keys)
  (general-evil-setup)
  (general-define-key
   :states '(normal insert motion emacs)
   :keymaps 'override
   :prefix-map 'tyrant-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (general-create-definer tyrant-def :keymaps 'tyrant-map)

  (tyrant-def "" nil)

  (general-create-definer despot-def
    :states '(normal insert motion emacs)
    :keymaps 'override
    :major-modes t
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")

  (despot-def "" nil)

  (general-def universal-argument-map "SPC u" 'universal-argument-more)

  (tyrant-def
      "SPC"     '("M-x" . execute-extended-command)
      "!"       '("shell cmd" . shell-command)
      "a"       '("agent shell" . agent-shell)
      "m"       (cons "files" (make-sparse-keymap))
      "m"       (cons "major mode" (make-sparse-keymap))
      "s"       (cons "search" (make-sparse-keymap))
      "t"       (cons "terminal" (make-sparse-keymap))

      "u"       (cons "universal arg" 'universal-argument)
      )
  )

(use-package evil
  :demand t
  :preface (setq evil-want-keybinding nil)
  :custom
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-C-i-jump t)
  (evil-undo-system 'undo-redo)
  (evil-shift-width 2 "same behavior for vim")
  (evil-complete-all-buffers nil)
  :config
  (defun +evil-kill-minibuffer () (interactive)
    (when (windowp (active-minibuffer-window)) (evil-ex-search-exit)))

  (add-hook 'mouse-leave-buffer-hook #'+evil-kill-minibuffer)
  (evil-mode))

(use-package evil-collection
  :after (evil)
  :config (evil-collection-init)
  :custom
  (evil-collection-elpaca-want-g-filters nil)
  (evil-collection-setup-minibuffer t "Add evil bindings to minibuffer")
  (evil-collection-company-use-tng t))

(use-package evil-nerd-commenter
  :after (general)
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  ;; :custom (general-define-key :state '(normal visual) "gc" 'evilnc-comment-operator)
  :general
  ([remap comment-line] #'evilnc-comment-or-uncomment-lines
   :keymaps 'prog-mode-map
   :states '(normal visual) "gc" 'evilnc-comment-operator))

(use-package evil-matchit :config (global-evil-matchit-mode 1))

;; TODO:
;; (use-package evil-lion :ensure (evil-lion :host github :repo "edkolev/evil-lion" :files ("*" (:exclude ".git"))) :config (evil-lion-mode))

(use-package evil-surround :config (global-evil-surround-mode 1))

(use-package evil-embrace
  :commands embrace-add-pair embrace-add-pair-regexp
  :after evil-surround
  :init (evil-embrace-enable-evil-surround-integration)
  :config (setq evil-embrace-show-help nil)
  :hook (org-mode . embrace-org-mode-hook)
  :hook (emace-lisp-mode . embrace-emacs-lisp-mode-hook))

(use-package evil-escape
  :commands evil-escape
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(treemacs-mode vterm-mode))
  (evil-define-key '(insert replace visual operator) 'global "\C-g" #'evil-escape))

(use-package evil-exchange :config (evil-exchange-install))

(use-package evil-traces :after evil :config (evil-traces-use-diff-faces) (evil-traces-mode))

(use-package evil-snipe
  :after evil
  :demand
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-iedit-state
  :after iedit
  :general
  (tyrant-def
    "s e" '(evil-iedit-state/iedit-mode :wk "iedit")
    "s q" '(evil-iedit-state/quit-iedit-mode :wk "quit iedit")))

(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package hydra :demand t)

(tyrant-def
  "b"       (cons "buffers" (make-sparse-keymap))
  "bb"      'switch-to-buffer
  "bc"      'consult-buffer
  "bi"      'ibuffer
  "bd"      'kill-current-buffer
  "bm"      'switch-to-messages-buffer
  "bs"      'scratch-buffer
  "bu"      'reopen-killed-buffer
  "bx"      'kill-buffer-and-window
  "b TAB"    '("last buffer" . alternate-buffer))

(use-package major-mode-hydra
  :after hydra
  :bind ("C-M-<return>" . major-mode-hydra)
  :config
  (major-mode-hydra-define emacs-lisp-mode nil
      ("Eval"
       (("b" eval-buffer "buffer")
        ("e" eval-defun "defun")
        ("r" eval-region "region")
        ("q" nil)))))

(use-package pretty-hydra
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:inherit highlight :reverse-video t)))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))
  ) 

(use-package use-package-hydra :after hydra)

(tyrant-def
  "h"       (cons "help" (make-sparse-keymap))
  "ha"      'apropos
  "hb"      'describe-bindings
  "hc"      'describe-char
  "hf"      'describe-function
  "hF"      'describe-face
  "hi"      'info-emacs-manual
  "hI"      'info-display-manual
  "hk"      'describe-key
  "hK"      'describe-keymap
  "hm"      'describe-mode
  "hM"      'woman
  "hp"      'describe-package
  "ht"      'describe-text-properties
  "hv"      'describe-variable
  "hP"      (cons "profiler" (make-sparse-keymap))
  "hPs"     'profiler-start
  "hPk"     'profiler-stop
  "hPr"     'profiler-report)

(tyrant-def
  "j"       (cons "jump" (make-sparse-keymap))
  "ji"      'imenu
  "jc"      'avy-goto-char
  "jg"      'avy-goto-char-2)

(tyrant-def
  "f"       (cons "files" (make-sparse-keymap))
  "fC"      '("copy-file" . write-file)
  "fD"      'delete-current-buffer-file
  "fE"      'sudo-edit
  "ff"      'find-file
  "fj"      'dired-jump
  "fJ"      'dired-jump-other-window
  "fo"      'open-file-or-directory-in-external-app
  "fr"      'read-only-mode
  "fR"      'rename-current-buffer-file
  "fd"   '((lambda (&optional arg) (interactive "P") (let ((buffer (when arg (current-buffer)))) (diff-buffer-with-file buffer))) :which-key "diff-with-file")

  "fe"   (cons "emacs" (make-sparse-keymap))
  ;; TODO:
  "fed"  '((lambda () (interactive)
            (find-file (expand-file-name "lisp/base.el" user-emacs-directory)))
           :which-key "config source")
  "feR"  '((lambda () (interactive)
            (load-file (expand-file-name "lisp/init.el" user-emacs-directory)))
           :which-key "reload config")
  "fp"   'find-function-at-point
  "fP"   'find-function
  "fR"   'rename-file-and-buffer
  "fs"   'save-buffer
  "fv"   'find-variable-at-point
  "fV"   'find-variable)

(tyrant-def
  "F"       (cons "frame" (make-sparse-keymap))
  "Fd"      'delete-frame
  "FD"      'delete-other-frames
  "Fn"      'make-frame
  "Fo"      'other-frame
  "FD" 'delete-other-frames
  "FF" 'select-frame-by-name
  "FO" 'other-frame-prefix
  "Fc" '(:ingore t :which-key "color")
  "Fcb" 'set-background-color
  "Fcc" 'set-cursor-color
  "Fcf" 'set-foreground-color
  "Ff" 'set-frame-font
  "Fm" 'make-frame-on-monitor
  "Fn" 'next-window-any-frame
  "Fo" 'other-frame
  "Fp" 'previous-window-any-frame
  "Fr" 'set-frame-name)

(tyrant-def
  "w"       (cons "windows" (make-sparse-keymap))
  "w TAB"   'alternate-window
  "w+"      'window-layout-toggle
  "w?" 'split-window-vertically
  "w=" 'balance-windows
  "w/" 'split-window-horizontally
  "wO" 'delete-other-windows
  "wX" '((lambda () (interactive) (call-interactively #'other-window) (kill-buffer-and-window)) :which-key "kill-other-buffer-and-window")
  "wd" 'delete-window
  "wh" 'windmove-left
  "wj" 'windmove-down
  "wk" 'windmove-up
  "wl" 'windmove-right
  "wo" 'other-window
  "wx" 'kill-buffer-and-window

  "wb"      'switch-to-minibuffer-window
  "wd"      'delete-window
  "wD"      'delete-other-windows
  "wm"      'toggle-maximize-buffer
  "wf"      'follow-mode
  "wh"      'evil-window-left
  "wH"      'evil-window-move-far-left
  "wj"      'evil-window-down
  "wJ"      'evil-window-move-very-bottom
  "wk"      'evil-window-up
  "wK"      'evil-window-move-very-top
  "wl"      'evil-window-right
  "wL"      'evil-window-move-far-right
  "wr"      'rotate-windows-forward
  "wR"      'rotate-windows-backward
  "ws"      'split-window-vertically
  "wS"      'split-window-vertically-and-focus
  "wt"      'toggle-current-window-dedication
  "wu"      'winner-undo
  "wU"      'winner-redo
  "wv"      'split-window-horizontally
  "wV"      'split-window-horizontally-and-focus)

(tyrant-def
  "p"       (cons "projects" project-prefix-map)
  "pt"      'project-open-in-tab)

(tyrant-def
  "T"       (cons "toggles" (make-sparse-keymap))
  "Ta"      'auto-fill-mode
  ;;"Td"      'toggle-debug-on-error
  "Tde"     'toggle-debug-on-error
  "Tdq"     'toggle-debug-on-quit
  "Tf"      'display-fill-column-indicator-mode
  "Tl"      'toggle-truncate-lines
  "Tm"      'flymake-mode
  "Tn"      'display-line-numbers-mode
  "Ts"      'flyspell-mode
  "Tw"      'whitespace-mode
  "TW"      'toggle-word-wrap)

(tyrant-def
  "l"       (cons "layouts" tab-prefix-map)
  "ld"      'tab-bar-close-tab
  "lD"      'tab-bar-close-other-tabs
  "lg"      'tab-bar-change-tab-group
  "lm"      'tab-bar-move-tab-to
  "lM"      'tab-bar-move-tab-to-group
  "ll"      'tab-bar-switch-to-tab
  "lR"      'tab-bar-rename-tab
  "lt"      'other-tab-prefix
  "lu"      'tab-bar-undo-close-tab
  "l TAB"   'tab-bar-switch-to-last-tab)

(tyrant-def
  "q"       (cons "quit" (make-sparse-keymap))
  "qd"      'restart-emacs-debug-init
  "qr"      'restart-emacs
  "qR"      'restart-emacs-without-desktop
  "qf"      'delete-frame
  "qq"      'save-buffers-kill-terminal
  "qQ"      'save-buffers-kill-emacs)

(use-package casual
  :defer t
  :general
  (:states 'normal
   :keymaps '(calc-mode-map calc-alg-map)
   "C-m" 'casual-calc-tmenu)
  (:states 'normal
   :keymaps 'dired-mode-map
   "C-o" 'casual-dired-tmenu))

;; 禁用一些GUI特性
(setq use-dialog-box nil)               ; 鼠标操作不使用对话框
(setq inhibit-default-init t)           ; 不加载 `default' 库
(setq inhibit-startup-screen t)         ; 不加载启动画面
(setq inhibit-startup-message t)        ; 不加载启动消息
(setq inhibit-startup-buffer-menu t)    ; 不显示缓冲区列表

;; 设置自动折行宽度为80个字符，默认值为70
(setq-default fill-column 80)

;; 设置大文件阈值为100MB，默认10MB
(setq large-file-warning-threshold 100000000)

;; 以16进制显示字节数
(setq display-raw-bytes-as-hex t)
;; 有输入时禁止 `fontification' 相关的函数钩子，能让滚动更顺滑
(setq redisplay-skip-fontification-on-input t)

;; 禁止响铃
(setq ring-bell-function 'ignore)

;; 在光标处而非鼠标所在位置粘贴
(setq mouse-yank-at-point t)

;; 拷贝粘贴设置
(setq select-enable-primary nil)        ; 选择文字时不拷贝
(setq select-enable-clipboard t)        ; 拷贝时使用剪贴板

;; 鼠标滚动设置
(setq scroll-step 2)
(setq scroll-margin 2)
(setq hscroll-step 2)
(setq hscroll-margin 2)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq scroll-preserve-screen-position 'always)

;; 对于高的行禁止自动垂直滚动
(setq auto-window-vscroll nil)

;; 设置新分屏打开的位置的阈值
(setq split-width-threshold (assoc-default 'width default-frame-alist))
(setq split-height-threshold nil)

;; TAB键设置，在Emacs里不使用TAB键，所有的TAB默认为4个空格
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; yes或no提示设置，通过下面这个函数设置当缓冲区名字匹配到预设的字符串时自动回答yes
(setq original-y-or-n-p 'y-or-n-p)
(defalias 'original-y-or-n-p (symbol-function 'y-or-n-p))
(defun default-yes-sometimes (prompt)
  "automatically say y when buffer name match following string"
  (if (or (string-match "has a running process" prompt)
          (string-match "does not exist; create" prompt)
          (string-match "modified; kill anyway" prompt)
          (string-match "Delete buffer using" prompt)
          (string-match "Kill buffer of" prompt)
          (string-match "still connected.  Kill it?" prompt)
          (string-match "Shutdown the client's kernel" prompt)
          (string-match "kill them and exit anyway" prompt)
          (string-match "Revert buffer from file" prompt)
          (string-match "Kill Dired buffer of" prompt)
          (string-match "delete buffer using" prompt)
          (string-match "Kill all pass entry" prompt)
          (string-match "for all cursors" prompt)
          (string-match "Do you want edit the entry" prompt))
      t
      (original-y-or-n-p prompt)))
(defalias 'yes-or-no-p 'default-yes-sometimes)
(defalias 'y-or-n-p 'default-yes-sometimes)

;; 设置剪贴板历史长度300，默认为60
(setq kill-ring-max 200)

;; 在剪贴板里不存储重复内容
(setq kill-do-not-save-duplicates t)

;; 设置位置记录长度为6，默认为16
;; 可以使用 `counsel-mark-ring' or `consult-mark' (C-x j) 来访问光标位置记录
;; 使用 C-x C-SPC 执行 `pop-global-mark' 直接跳转到上一个全局位置处
;; 使用 C-u C-SPC 跳转到本地位置处
(setq mark-ring-max 6)
(setq global-mark-ring-max 6)

;; 设置 emacs-lisp 的限制
(setq max-lisp-eval-depth 10000)        ; 默认值为 800
(setq max-specpdl-size 10000)           ; 默认值为 1600

;; 启用 `list-timers', `list-threads' 这两个命令
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

;; 在命令行里支持鼠标
(xterm-mouse-mode 1)

;; 退出Emacs时进行确认
;; (setq confirm-kill-emacs 'y-or-n-p)

;; 在模式栏上显示当前光标的列号
(column-number-mode t)

(defun fonts-installed (&rest font-list) (reverse (cl-intersection font-list (font-family-list) :test #'equal)))

(use-package fontaine
  :defer t
  :init
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
  ;; set Chinese font scale
  (setq face-font-rescale-alist `(
                                  ("LXGW WenKai"          . 1.15)
                                  ("Symbola"             . 1.3)
                                  ("Microsoft YaHei"     . 1.2)
                                  ("WenQuanYi Zen Hei"   . 1.2)
                                  ("Sarasa Mono SC Nerd" . 1.2)
                                  ("PingFang SC"         . 1.16)
                                  ("Lantinghei SC"       . 1.16)
                                  ("Kaiti SC"            . 1.16)
                                  ("Yuanti SC"           . 1.16)
                                  ("Apple Color Emoji"   . 0.91)))
  ;; In a daemon there is no graphic frame during init, so fonts must be
  ;; applied when one appears.  Applying on `elpaca-after-init-hook'
  ;; (non-daemon) also wins over any theme that redefines the `default'
  ;; face and would otherwise drop the family to the fontconfig fallback.
  (defun emacy-apply-fonts (&optional _frame)
    "Apply the fontaine preset plus emoji and CJK fontsets on graphic displays."
    ;; Neomacs renders fonts through its own engine; fontaine's X font
    ;; machinery does not apply there.
    (when (and (not (and (boundp 'neomacs?) neomacs?))
               (require 'fontaine nil t)
               (seq-filter #'display-graphic-p (frame-list)))
      (fontaine-set-preset (or (ignore-errors (fontaine-restore-latest-preset)) 'regular))
      (let ((emoji (car (fonts-installed "Noto Emoji" "Symbola" "Apple Color Emoji" "Noto Color Emoji" "Segoe UI Emoji")))
            (cjk (car (fonts-installed "LXGW WenKai" "霞鹜文楷" "Sarasa Gothic SC" "更纱黑体 SC"))))
        (when emoji
          (set-fontset-font t (if (version< emacs-version "28.1") '(#x1f300 . #x1fad0) 'emoji) emoji))
        (when cjk
          (dolist (charset '(kana han symbol cjk-misc bopomofo))
            (set-fontset-font t charset (font-spec :family cjk))
            (dolist (frame (seq-filter #'display-graphic-p (frame-list)))
              ;; Frames created before this call hold a copy of the
              ;; default fontset; patch those fontsets directly too.
              (set-fontset-font (frame-parameter frame 'font) charset (font-spec :family cjk))))))))
  (add-hook 'elpaca-after-init-hook #'emacy-apply-fonts)
  (add-hook 'after-make-frame-functions #'emacy-apply-fonts))

(use-package fontify-face)

(use-package transpose-frame
  :general
  (tyrant-def
    "w [" 'transpose-frame
    "w ]" 'rotate-frame))

(use-package ace-window)

(use-package shackle
  :hook (elpaca-after-init . shackle-mode)
  :init
  (setq shackle-lighter "")
  (setq shackle-select-reused-windows nil) ; default nil
  (setq shackle-default-alignment 'below)  ; default below
  (setq shackle-default-size 0.4)          ; default 0.5
  (setq shackle-rules
        ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
        '((compilation-mode              :ignore t)
          ("\\*Async Shell.*\\*" :regexp t :ignore t)
          ("\\*corfu.*\\*"       :regexp t :ignore t)
          ("*eshell*"                    :select t                          :size 0.4  :align t     :popup t)
          (helpful-mode                  :select t                          :size 0.6  :align right :popup t)
          ("*Messages*"                  :select t                          :size 0.4  :align t     :popup t)
          ("*Calendar*"                  :select t                          :size 0.3  :align t     :popup t)
          ("*info*"                      :select t                                                  :same t)
          (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
          (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
          )))

(use-feature winner
  :hook (after-init . winner-mode)
  :commands (winner-undo winner-redo)
  :config
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*")))

(use-feature savehist
  :hook (after-init . savehist-mode)
  :config
  ;; Allow commands in minibuffers, will affect `dired-do-dired-do-find-regexp-and-replace' command:
  (setq enable-recursive-minibuffers t)
  (setq history-length 1000)
  (setq savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history))
  (setq savehist-autosave-interval 300))

(use-feature saveplace :hook (after-init . save-place-mode))

(use-feature recentf
  :defines no-littering-etc-directory no-littering-var-directory
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  ;; `recentf-add-file' will apply handlers first, then call `string-prefix-p'
  ;; to check if it can be pushed to recentf list.
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-exclude `(,@(cl-loop for f in `(,package-user-dir
                                           ,no-littering-var-directory
                                           ,no-littering-etc-directory)
                                collect (abbreviate-file-name f))
                     ;; Folders on MacOS start
                     "^/private/tmp/"
                     "^/var/folders/"
                     ;; Folders on MacOS end
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "^/tmp/"
                     "/ssh\\(x\\)?:"
                     "/su\\(do\\)?:"
                     "^/usr/include/"
                     "/TAGS\\'"
                     "COMMIT_EDITMSG\\'")))

(use-package vundo)

(use-package doom-modeline
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

(use-package minions :hook (elpaca-after-init . minions-mode))

(use-package keycast
  :hook (elpaca-after-init . keycast-mode)
  :config
  ;; set for doom-modeline support
  ;; With the latest change 72d9add, mode-line-keycast needs to be modified to keycast-mode-line.
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (fix for use with doom-mode-line)."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" keycast-mode-line "  ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (delete '("" keycast-mode-line "  ") global-mode-string))
      ))

  (dolist (input '(self-insert-command
                  org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p
                  mouse-movement-p
                  mwheel-scroll))
    (add-to-list 'keycast-substitute-alist `(,event nil)))

  (setq keycast-log-format "%-20K%C\n")
  (setq keycast-log-frame-alist
        '((minibuffer . nil)))
  (setq keycast-log-newest-first t))

(use-feature autorevert
  :hook (after-init . global-auto-revert-mode)
  :bind ("s-u" . revert-buffer)
  :custom
  (auto-revert-interval 10)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t))

(use-package iedit)

(defun emacy--unspecify-nil-attributes (plist)
  "Replace nil values in face attribute PLIST with `unspecified'."
  (let (result)
    (while plist
      (push (pop plist) result)
      (let ((value (pop plist)))
        (push (if (null value) 'unspecified value) result)))
    (nreverse result)))

(defun emacy--repair-face-spec (spec)
  "Replace nil attribute values in face SPEC with `unspecified'."
  (if (listp spec)
      (mapcar
       (lambda (entry)
         (cond
          ;; (DISPLAY :attribute value ...)
          ((and (consp entry) (keywordp (car (cdr entry))))
           (cons (car entry)
                 (emacy--unspecify-nil-attributes (cdr entry))))
          ;; (DISPLAY (:attribute value ...))
          ((and (consp entry) (consp (cdr entry))
                (consp (car (cdr entry)))
                (keywordp (caar (cdr entry))))
           (cons (car entry)
                 (cons (emacy--unspecify-nil-attributes (car (cdr entry)))
                       (cdr (cdr entry)))))
          (t entry)))
       spec)
    spec))

(defun emacy--repair-theme-faces (theme)
  "Replace nil attribute values in THEME's face specs with `unspecified'.
Emacs 31 refuses nil face attribute values and warns as soon as such a
spec is applied; `unspecified' expresses the same intent legally.
Only THEME's `theme-settings' are patched: `enable-theme' copies them
into the faces, so the faces pick up the repaired specs."
  (let ((settings (get theme 'theme-settings)))
    (when settings
      (dolist (setting settings)
        (when (and (eq (nth 0 setting) 'theme-face)
                   (eq (nth 2 setting) theme))
          (setcar (nthcdr 3 setting)
                  (emacy--repair-face-spec (nth 3 setting))))))))

(use-package zenburn-theme
  :config
  ;; Load the theme without enabling it: zenburn's face specs hold nil
  ;; attribute values, which Emacs 31 refuses with a warning as soon as
  ;; they are applied.  `load-theme' takes NO-CONFIRM as its second
  ;; argument and NO-ENABLE as its third.
  (load-theme 'zenburn t t)
  (emacy--repair-theme-faces 'zenburn)
  (enable-theme 'zenburn))

(use-package dashboard
  :demand t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t
        dashboard-items '((recents . 9)
                          (bookmarks . 6)
                          (projects . 6)
                          (agenda . 9))
        dashboard-set-file-icons t
        dashboard-set-heading-icons t))

(use-package page-break-lines :hook (dashboard-mode . page-break-lines-mode))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-ibuffer :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("e" "~/.emacs.d"                  "Emacs")
     ("p" "~/Projects/"                 "Projects")
     ("d" "~/Downloads/"                "Downloads")
     ("t" "~/.local/share/Trash/files/" "Trash")))
  :config
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-mode-line-height 10)
  (setq dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-subtree-state-style 'nerd)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-peek-mode)
  (dirvish-side-follow-mode))

(use-package perspective
  :custom (persp-mode-prefix-key (kbd "C-c TAB"))
  :init (persp-mode)
  :general (tyrant-def "TAB"     (cons "layouts" perspective-map)))

(use-package yasnippet :config (yas-global-mode 1))

;; Replaces the dropped lib/yasnippet-snippets submodule; its autoload
;; adds the snippets directory to `yas-snippet-dirs'.
(use-package yasnippet-snippets :after yasnippet)

(use-package tempel)

(use-package tempel-collection
  :after tempel)

(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word))
  :hook (elpaca-after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

(use-package dumb-jump
  :bind (("M-g j" . dumb-jump-hydra/body))
  :config
  (defhydra dumb-jump-hydra (:color blue :columns 3)
      "Dumb Jump"
      ("j" dumb-jump-go "Go")
      ("o" dumb-jump-go-other-window "Other window")
      ("e" dumb-jump-go-prefer-external "Go external")
      ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
      ("i" dumb-jump-go-prompt "Prompt")
      ("l" dumb-jump-quick-look "Quick look")
      ("b" dumb-jump-back "Back")))

(use-package hl-todo :config (global-hl-todo-mode))
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement if_statement with_statement while_statement))))

(use-package symbol-overlay
  :hook
  ((prog-mode . symbol-overlay-mode))
  :bind
  (("M-i" . 'symbol-overlay-put)
   ("M-n" . 'symbol-overlay-jump-next)
   ("M-p" . 'symbol-overlay-jump-prev)
   ("M-N" . 'symbol-overlay-switch-forward)
   ("M-P" . 'symbol-overlay-switch-backward)
   ("M-I" . 'symbol-overlay-remove-all)))

(use-package diff-hl
  :custom (diff-hl-draw-borders nil)
  :custom-face
  (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
  (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
  (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
  :bind (:map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         ;; (dired-mode . diff-hl-dired-mode)
         )
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (with-no-warnings
    (defun my-diff-hl-fringe-bmp-function (_type _pos)
      "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
      (define-fringe-bitmap 'my-diff-hl-bmp
        (vector (if linux? #b11111100 #b11100000))
        1 8
        '(center t)))
    (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

    (unless (display-graphic-p)
      ;; Fall back to the display margin since the fringe is unavailable in tty
      (diff-hl-margin-mode 1)
      ;; Avoid restoring `diff-hl-margin-mode'
      (with-eval-after-load 'desktop
        (add-to-list 'desktop-minor-mode-table
                     '(diff-hl-margin-mode nil))))

    ;; Integration with magit
    (with-eval-after-load 'magit
      (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

(use-feature eshell
  :functions eshell/alias
  :hook ((eshell-mode . (lambda () (term-mode-common-init) (visual-line-mode 1))))
  :config
  (defun term-mode-common-init ()
    (setq-local scroll-margin 0)
    (setq-local truncate-lines t))

  (defalias 'eshell/vi 'find-file)
  (defalias 'eshell/vim 'find-file)

  (defun eshell/bat (file)
    "cat FILE with syntax highlight."
    (with-temp-buffer
      (insert-file-contents file)
      (let ((buffer-file-name file))
        (delay-mode-hooks
          (set-auto-mode)
          (font-lock-ensure)))
      (buffer-string)))
  (defalias 'eshell/cat 'eshell/bat)

  ;; 交互式进入目录
  (defun eshell/z ()
    "cd to directory with completion."
    (let ((dir (completing-read "Directory: " (ring-elements eshell-last-dir-ring) nil t)))
      (eshell/cd dir)))

  ;; 查找文件
  (defun eshell/f (filename &optional dir)
    "Search for files matching FILENAME in either DIR or the
current directory."
    (let ((cmd (concat
                ;; using find
                (executable-find "find")
                " " (or dir ".")
                " -not -path '*/.git*'"            ; ignore .git directory
                " -and -not -path 'build'"         ; ignore cmake build directory
                " -and -not -path '*/eln-cache*'"  ; ignore eln cache
                " -and -type f -and -iname "
                "'*" filename "*'")))
      (eshell-command-result cmd)))

  :custom
  (eshell-banner-message
   '(format "%s %s\n"
            (propertize (format " %s " (string-trim (buffer-name)))
                        'face 'mode-line-highlight)
            (propertize (current-time-string)
                        'face 'font-lock-keyword-face)))
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-kill-on-exit t)
  (eshell-kill-processes-on-exit t)
  ;; Don't record command in history if starts with whitespace
  (eshell-input-filter 'eshell-input-filter-initial-space)
  (eshell-error-if-no-glob t)
  (eshell-glob-case-insensitive t)
  ;; set scripts
  (eshell-rc-script (locate-user-emacs-file "etc/eshell/profile"))
  (eshell-login-script (locate-user-emacs-file "etc/eshell/login")))

(use-feature em-hist
  :defer t
  :custom
  (eshell-history-size 1024)
  (eshell-hist-ignoredups t)
  (eshell-save-history-on-exit t))

(use-feature em-rebind
  :commands eshell-delchar-or-maybe-eof)

(use-feature esh-mode
  :bind (:map eshell-mode-map
              ("C-d" . eshell-delchar-or-maybe-eof)
              ("C-r" . consult-history)
              ("C-l" . eshell/clear)))

;; (use-package eshell-syntax-highlighting
;;   :after esh-mode
;;   :ensure t
;;   :hook (eshell-mode . eshell-syntax-highlighting-global-mode)
;;   :custom-face
;;   (eshell-syntax-highlighting-shell-command-face ((t (:foreground "#7cc77f" :bold t)))))

(use-package aweshell
  :general
  (tyrant-def
    "t e" 'aweshell-dedicated-toggle
    "t a" 'aweshell-new
    "t s" 'aweshell-sudo-toggle
    "t j" 'aweshell-prev
    "t k" 'aweshell-next
    "t x" 'aweshell-dedicated-close))


(use-package vterm
  :defer t
  :unless (or windows? neomacs?))

(use-package multi-vterm
  :defer t
  :unless (or windows? neomacs?))

;; The :general bindings must install at startup; gating them with
;; :after vterm would leave them unset because vterm itself is deferred.
(use-package vterm-toggle
  :defer t
  :unless (or windows? neomacs?)
  :general
  (tyrant-def
    "t t" 'vterm-toggle
    "t c" 'multi-vterm
    "t p" 'multi-vterm-project
    "t d" 'multi-vterm-dedicated-toggle
    "t n" 'vterm-toggle-forward
    "t p" 'vterm-toggle-backward
    )
  )

(use-package sideline
  :hook
  ((prog-mode . sideline-mode))
  :custom
  ;; (sideline-backends-left-skip-current-line t)
  ;; (sideline-backends-right-skip-current-line t)
  ;; (sideline-order-left 'down)
  ;; (sideline-order-right 'up)
  (sideline-display-backend-name t))

(use-package sideline-eglot
  :init
  (setq sideline-backends-left '(sideline-eglot)))

(use-package sideline-flymake
  :init
  (setq sideline-backends-right '(sideline-flymake)))

(use-package blamer
  :defer 20
  :custom
  (blamer-view 'overlay)
  (blamer-idle-time 0.3)
  (blamer-min-offset 21)
  (blamer-show-avatar-p nil)
  (blamer-smart-background-p t)
  ;; :custom-face
  ;; (blamer-face ((t :foreground "#7a88cf"
  ;;                   :background nil
  ;;                   :height 140
  ;;                   :italic t)))
  :config
  (global-blamer-mode 1))

(use-package combobulate
  :preface
  (setq combobulate-key-prefix "C-c o")
  :hook
  (((python-ts-mode js-ts-mode typescript-ts-mode tsx-ts-mode json-ts-mode html-ts-mode css-ts-mode) . combobulate-mode)))

(use-package vertico
  :demand
  :config
  (vertico-mode)
  (setq vertico-scroll-margin 0
        vertico-resize t
        vertico-cycle t))

;; A few more useful configurations...
(use-feature emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package pinyinlib)

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless partial-completion basic))
  (setq orderless-component-separator "[ &]") ; & is for company because space will break completion
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion))))
  :config
  (defun completion--regex-pinyin (str) (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; minibuffer helpful annotations
(use-package marginalia
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package consult
  :after org
  :bind (([remap goto-line]                     . consult-goto-line)
         ([remap isearch-forward]               . consult-line-symbol-at-point) ; my-consult-ripgrep-or-line
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap apropos]                       . consult-apropos)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap multi-occur]                   . consult-multi-occur)
         ([remap recentf-open-files]            . consult-recent-file)
         ("C-x j"                               . consult-mark)
         ("C-c g"                               . consult-ripgrep)
         ("C-c f"                               . consult-find)
         ("\e\ef"                               . consult-locate) ; need to enable locate first
         ("C-c n h"                             . my/consult-find-org-headings)
         :map org-mode-map
         ("C-c C-j"                             . consult-org-heading)
         :map minibuffer-local-map
         ("C-r"                                 . consult-history)
         :map isearch-mode-map
         ("C-;"                                 . consult-line)
         :map prog-mode-map
         ("C-c C-j"                             . consult-outline)
         )
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; MacOS locate doesn't support `--ignore-case --existing' args.
  (setq consult-locate-args (pcase system-type
                              ('gnu/linux "locate --ignore-case --existing --regex")
                              ('darwin "mdfind -name")))
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-recent-file consult-source-project-recent-file consult-source-bookmark
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (setq consult-project-root-function #'projectile-project-root)

  ;; search all org file headings under a directory, see:
  ;; https://emacs-china.org/t/org-files-heading-entry/20830/4
  (defun my/consult-find-org-headings (&optional match)
    "find headngs in all org files."
    (interactive)
    (consult-org-heading match (directory-files org-directory t "^[0-9]\\{8\\}.+\\.org$")))

  ;; Use `consult-ripgrep' instead of `consult-line' in large buffers
  (defun consult-line-symbol-at-point ()
    "Consult line the synbol where the point is"
    (interactive)
    (consult-line (thing-at-point 'symbol))))

(use-package consult-todo)

;; `projectile-project-root' backs `consult-project-root-function'; the
;; package has to be installed for the autoload to resolve.
(use-package projectile)

(use-package embark
  :demand t
  :general
  (:keymaps 'override
   "C-;" 'embark-dwim
   :states '(normal insert motion emacs)
   "C-." 'embark-act)

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :init
  (global-corfu-mode))

(use-feature emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package cape
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  ;; (add-hook 'completion-at-point-functions #'cape-keyword)
  ;;(add-hook 'completion-at-point-functions #'cape-tex)
  ;;(add-hook 'completion-at-point-functions #'cape-sgml)
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345)
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  ;;(add-hook 'completion-at-point-functions #'cape-dict)
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-hook 'completion-at-point-functions #'cape-line)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  )

(setq org-directory  "~/Notes/"
      org-archive-location (concat org-directory "/archive/%s::")
      org-contacts-files (list (concat org-directory "/archive/contacts.org"))
      org-agenda-files (list org-directory (concat org-directory "work/"))
      deft-directory org-directory
      org-roam-directory org-directory
      org-brain-path org-directory
      org-roam-file-exclude-regexp ".*/bak/.*"
      org-brain-visualize-default-choices 'all
      rmh-elfeed-org-files (list (concat org-directory "content/feeds.org")))

(setq org-fontify-whole-heading-line t
      org-fontify-quote-and-verse-blocks t
      org-hide-macro-markers t
      org-startup-indented t
      org-adapt-indentation t
      org-startup-with-inline-images t
      org-startup-with-latex-preview t)

(setq org-log-done 'time)
;; (setq org-priority-lowest ?E)
(setq org-contacts-icon-use-gravatar nil)
(setq org-superstar-headline-bullets-list '("☯", "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
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

(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Notes/inbox.org")
         "* TODO %^{Name}")
        ("w" "Work" entry (file "~/Notes/work/task.org")
         "* TODO %^{Name}")
        ("j" "Journal" entry (file+olp+datetree "~/Notes/journal/index.org")
         "* %U - %^{heading} %^g\n %?\n")))

(use-package org-contrib)

(setq org-agenda-archives-mode t)
(setq org-agenda-custom-commands
      '(("w" "Works"
         ((agenda "" ((tags "work")))
          (todo "NEXT|TODO")
          (todo "PEND")
          (todo "WILL"))
         ((org-super-agenda-groups
           '((:discard (:not (:tag "work"))))))
         )))
(setq org-export-select-tags '("Publish" "Public" "export")
      org-publish-project-alist
      '(("content"
          :base-directory "~/Notes/content/"
          :publishing-directory "~/Notes/publish/"
          ;; :publishing-function (org-org-publish-to-org org-md-publish-to-md)
          :publishing-function org-org-publish-to-org
          :select-tags ("Publish" "Public" "Export" "export" "publish" "public")
          :exclude-tags ("Private" "private")
          :recursive t
          :with-broken-links t
          :with-toc nil)))

(use-package org-brain
  :init
  (setq org-brain-path org-directory)
  ;; For Evil users
  (with-eval-after-load 'evil (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (bind-key "C-c b" 'org-brain-prefix-map org-mode-map)
  ;; (setq org-id-track-globally t)
  (setq org-id-locations-file "~/Notes/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (push '("b" "Brain" plain (function org-brain-goto-end) "* %i%?" :empty-lines 1) org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

;; Allows you to edit entries directly from org-brain-visualize
;; (use-package polymode
;;   :config
;;   (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))

(use-package org-super-agenda
  :hook (after-init . org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
        '((:name "Next" :time-grid t :todo "NEXT")
          (:name "Important" :priority "A")
          (:todo "TODO"))))

(use-package elfeed)

(use-package elfeed-org
  :config
  (setq rmh-elfeed-org-files (list "~/Notes/feeds.org"))
  (elfeed-org))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-inside-latex t))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config
  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
  (setq org-modern-star 'replace)
  (setq org-modern-replace-stars "☰☱☲☳☴☵☶☷")

  ;; (setq org-modern-fold-stars '(("☯" . "☰ ") ("☯" . "☱ ") ("☯" . "☲ ") ("☯" . "☳") ("☯" . "☴") ("☯" . "☵") ("☯" . "☶") ("☯" . "☷")))

  (setq org-agenda-tags-column 0
        org-agenda-block-separator ?-
        org-agenda-time-grid '((daily today require-timed) (800 1000 1200 1400 1600 1800 2000) " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string "◀── now ─────────────────────────────────────────────────")

  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  ;; ;; Add frame borders and window dividers
  ;; (modify-all-frames-parameters
  ;;   '((right-divider-width . 40)
  ;;     (internal-border-width . 40)))
  ;; (dolist (face '(window-divider
  ;;                 window-divider-first-pixel
  ;;                 window-divider-last-pixel))
  ;;   (face-spec-reset-face face)
  ;;   (set-face-foreground face (face-attribute 'default :background)))
  ;; (set-face-background 'fringe (face-attribute 'default :background))
  )

(use-package org-roam
  :defer t
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

(use-package org-roam-ui
  :defer t
  :commands (org-roam-ui-open))

(setq deft-recursive t
      deft-recursive-ignore-dir-regexp (rx (or "." ".." "logseq") eol))

;; (use-package org-fragtog :hook (org-mode . org-fragtog-mode))

;; (use-package tex :elpaca auctex)

(use-package mpvi 
  :general
  (tyrant-def))

(use-package nushell-ts-babel
  :after org-contrib
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((nushell . t))))

(define-derived-mode nushell-mode prog-mode "nusehll mode")

(add-hook 'nushell-ts-mode-hook
          (lambda ()
            (corfu-mode 1)
            (highlight-parentheses-mode 1)
            (electric-pair-local-mode 1)
            (electric-indent-local-mode 1)))

(use-package racket-mode)

(use-package ob-racket
  :after org
  :config
  (add-hook 'ob-racket-pre-runtime-library-load-hook #'ob-racket-raco-make-runtime-library)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((racket . t))))

(use-package markdown-mode)

(use-feature treesit
  :config (setq treesit-font-lock-level 4)
  :init
  (setq treesit-language-source-alist
        '((nu . ("https://github.com/nushell/tree-sitter-nu/"))
          (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
          (org . ("https://github.com/milisims/tree-sitter-org"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (zig . ("https://github.com/GrayJack/tree-sitter-zig"))
          ))

  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (cmake-mode      . cmake-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (css-mode        . css-ts-mode)
          (js-mode         . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (python-mode     . python-ts-mode)
          (nushell-mode    . nushell-ts-mode)
          (sh-mode         . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)))
  (add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
  ;; Compiled grammar libraries live under var/treesit, next to the other
  ;; generated artifacts.
  (setq treesit-extra-load-path (list (expand-file-name "treesit" emacy-var-directory))))

(defun emacy--treesit-install-directory (orig-fun &rest args)
  "Install tree-sitter grammars into var/treesit.
`treesit-install-language-grammar' defaults to the \"tree-sitter\"
directory in the configuration root when OUT-DIR is nil; keep the
generated grammars with the other artifacts instead."
  (if (null (car (last args)))
      (apply orig-fun
             (append args (list (expand-file-name "treesit/" emacy-var-directory))))
    (apply orig-fun args)))

(advice-add 'treesit-install-language-grammar :around #'emacy--treesit-install-directory)

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package treesit-fold
  :hook
  ((prog-mode . treesit-fold-mode)))

(use-package magit
  :general
  (tyrant-def "g g" 'magit))

(use-package git-timemachine)

(use-package editorconfig :config (editorconfig-mode 1))

(use-package parinfer-rust-mode
  :hook ((emacs-lisp-mode lisp-mode) . parinfer-rust-mode)
  :hook ((emacs-lisp-mode lisp-mode) . (lambda () (electric-pair-mode -1)))

  :init
  (setq parinfer-rust-auto-download t
        parinfer-rust-library-directory (expand-file-name "parinfer-rust/" emacy-var-directory)))

(use-feature eglot
  :hook ((prog-mode . (lambda () (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode) (eglot-ensure)))))
  :init
  (setq read-process-output-max (* 1024 1024))
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5))

(use-package consult-eglot
  :bind
  (:map eglot-mode-map
        ("C-M-." . consult-eglot-symbols)))

(when (executable-find "emacs-lsp-booster")
  (use-package eglot-booster
    :after eglot
    :autoload eglot-booster-mode
    :init (eglot-booster-mode 1)))

(use-package css-mode :ensure nil :init (setq css-indent-offset 2))
;; (use-package scss-mode :init (setq scss-compile-at-save nil))
(use-feature less-css-mode)
(use-package json-mode)

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode)))

(use-package prettier
  :diminish
  :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
  :init (setq prettier-pre-warm 'none))

(use-package typescript-mode :mode ("\\.ts[x]\\'" . typescript-mode))

(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Adds node_modules/.bin directory to `exec_path'
(use-package add-node-modules-path :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(use-package jsdoc)

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package restclient-test
    :diminish
    :hook (restclient-mode . restclient-test-mode)))

(use-feature flymake
  :hook (prog-mode . flymake-mode)
  ;; :hook (flymake-mode . (lambda ()
  ;;                         (setq eldoc-documentation-functions
  ;;                               (cons 'flymake-eldoc-function
  ;;                                     (delq 'flymake-eldoc-function
  ;;                                           eldoc-documentation-functions)))))
  ;; :init (setq elisp-flymake-byte-compile-load-path (cons "./" load-path))
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))

(use-package keyfreq
  :hook 
  (after-init . keyfreq-mode )
  (after-init . keyfreq-autosave-mode)
  :init
  (setq keyfreq-excluded-commands 
        '(self-insert-command
          abort-recursive-edit
          forward-char
          backward-char
          previous-line
          next-line)))

(use-package rime
  :defer t
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-user-data-dir "~/Dotfiles/config/rime")
  (rime-disable-predicates '(rime-predicate-after-alphabet-char-p ;; 在英文字符串之后（必须为以字母开头的英文字符串）
                             ;; rime-predicate-after-ascii-char-p ;; 任意英文字符后
                             rime-predicate-prog-in-code-p ;; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
                             rime-predicate-in-code-string-p ;; 在代码的字符串中，不含注释的字符串。
                             rime-predicate-evil-mode-p ;; 在 evil-mode 的非编辑状态下
                             rime-predicate-hydra-p ;; 如果激活了一个 hydra keymap
                             ;; rime-predicate-ace-window-p ;; 激活 ace-window-mode
                             ;; rime-predicate-current-input-punctuation-p ;; 当要输入的是符号时
                             rime-predicate-punctuation-after-space-cc-p ;; 当要在中文字符且有空格之后输入符号时
                             rime-predicate-punctuation-after-ascii-p ;; 当要在任意英文字符之后输入符号时
                             rime-predicate-punctuation-line-begin-p ;; 在行首要输入符号时
                             rime-predicate-space-after-ascii-p ;; 在任意英文字符且有空格之后
                             rime-predicate-space-after-cc-p ;; 在中文字符且有空格之后
                             rime-predicate-current-uppercase-letter-p ;; 将要输入的为大写字母时
                             rime-predicate-tex-math-or-command-p));; 在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
  :bind
  (:map rime-mode-map ("C-," . 'rime-force-enable)))

;; AI agents over the Agent Client Protocol --------------------------------
;; `agent-shell' talks to agents such as OpenCode, Claude Code, Gemini CLI
;; and Codex.  Each agent ships a CLI; the list below keeps only the agents
;; whose CLI is actually installed.
(use-package agent-shell
  :after shell-maker
  :config
  (setq agent-shell-agent-configs
        (lambda ()
          (seq-filter
           (lambda (maker)
             (when-let* ((config (funcall maker))
                         (client-maker (map-elt config :client-maker))
                         (client (ignore-errors
                                   (funcall client-maker (current-buffer))))
                         (command (map-elt client :command)))
               (executable-find command)))
           (agent-shell-default-agent-config-makers)))))

;; Text application framework ----------------------------------------------
;; ETAF builds Emacs text applications out of retained components; it needs
;; the `ecss' and `ebox' packages, which Elpaca installs from GitHub
;; (see lisp/packages.el).
(use-package etaf)

(provide 'emacy-base)
;;; base.el ends here
