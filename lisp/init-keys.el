;;; init-keys.el --- Emacs pre-initialization config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

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

(use-package general
  :demand t
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
  
      "a"       (cons "applications" (make-sparse-keymap))
      "ac"      'calc-dispatch
      "ap"      'list-processes
      "aP"      'proced
  
      "ae"      (cons "elpaca" (make-sparse-keymap))
      "aeb" 'elpaca-browse
      "aer"  '((lambda () (interactive) (let ((current-prefix-arg (not current-prefix-arg))) (call-interactively #'elpaca-rebuild))) :which-key "rebuild")
      "aem" 'elpaca-manager
      "ael" 'elpaca-log
      "aei" 'elpaca-info
      "aeI" '((lambda () (interactive) (info "Elpaca")) :which-key "elpaca-info")
      "aes" 'elpaca-status
      "aet" 'elpaca-try
      "aev" 'elpaca-visit
  
      "b"       (cons "buffers" (make-sparse-keymap))
      "bb"      'switch-to-buffer
      "bc"      'consult-buffer
      "bi"      'ibuffer
      "bd"      'kill-current-buffer
      "bm"      'switch-to-messages-buffer
      "bs"      'scratch-buffer
      "bu"      'reopen-killed-buffer
      "bx"      'kill-buffer-and-window
      "b TAB"    '("last buffer" . alternate-buffer)
  
      "c"       (cons "code" (make-sparse-keymap))
      "cb"      'flymake-show-buffer-diagnostics
      "cc"      'compile
      "cd"      'lsp-bridge-find-def
      "cD"      'lsp-bridge-find-references
      "cn"      'next-error
      "cp"      'previous-error
      "cr"      'recompile
      "cx"      'kill-compilation
      "c="      'indent-region-or-buffer
  
      "f"       (cons "files" (make-sparse-keymap))
      "fC"      '("copy-file" . write-file)
      "fD"      'delete-current-buffer-file
      ;; "fe"      'find-library
      "fE"      'sudo-edit
      "ff"      'find-file
      "fj"      'dired-jump
      "fJ"      'dired-jump-other-window
      "fo"      'open-file-or-directory-in-external-app
      "fr"      'read-only-mode
      "fR"      'rename-current-buffer-file
      "fd"   '((lambda (&optional arg) (interactive "P") (let ((buffer (when arg (current-buffer)))) (diff-buffer-with-file buffer))) :which-key "diff-with-file")
  
      "fe"   (cons "emacs" (make-sparse-keymap))
      "fed"  '((lambda () (interactive) (find-file-existing literate-file) (widen)) :which-key "dotfile")
      "feR"  '((lambda () (interactive) (load-file user-init-file)) :which-key "reload-init.el")
      "fet"  '((lambda () (interactive)
                (save-restriction (widen) (check-parens) (org-babel-tangle-file literate-file))
                (load-file "~/Projects/Emacy/profiles/emacy/init.el"))
               :which-key "tangle/reload-init.el")
      "fl"   '((lambda (&optional arg) (interactive "P") (call-interactively (if arg #'find-library-other-window #'find-library))) :which-key "+find-library")
      "fp"   'find-function-at-point
      "fP"   'find-function
      "fR"   'rename-file-and-buffer
      "fs"   'save-buffer
      "fv"   'find-variable-at-point
      "fV"   'find-variable
      ;; "fv"      (cons "variables" (make-sparse-keymap))
      ;; "fvd"     'add-dir-local-variable
      ;; "fvf"     'add-file-local-variable
      ;; "fvp"     'add-file-local-variable-prop-line
  
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
      "Fr" 'set-frame-name
  
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
      "hPr"     'profiler-report
  
      "j"       (cons "jump" (make-sparse-keymap))
      "ji"      'imenu
      "jg"      'avy-goto-char-2
  
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
      "l TAB"   'tab-bar-switch-to-last-tab
  
      "m"       (cons "major mode" (make-sparse-keymap))
  
      "p"       (cons "projects" project-prefix-map)
      "pt"      'project-open-in-tab
  
      "q"       (cons "quit" (make-sparse-keymap))
      "qd"      'restart-emacs-debug-init
      "qr"      'restart-emacs
      "qR"      'restart-emacs-without-desktop
      "qf"      'delete-frame
      "qq"      'save-buffers-kill-terminal
      "qQ"      'save-buffers-kill-emacs
  
      "s"       (cons "search" (make-sparse-keymap))
      ;; "sb"      'flyspell-buffer
      ;; "sn"      'flyspell-goto-next-error
      ;; "sr"      'flyspell-region
  
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
      "TW"      'toggle-word-wrap
  
      "u"       '("universal arg" . universal-argument)
  
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
      "wV"      'split-window-horizontally-and-focus))

(use-package hydra
  :demand t
  :config
  (defun hydra-move-split-left (arg)
    "Move window split left."
    (interactive "p")
    (if (let* ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))
  
  (defun hydra-move-split-right (arg)
    "Move window split right."
    (interactive "p")
    (if (let* ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))
  
  (defun hydra-move-split-up (arg)
    "Move window split up."
    (interactive "p")
    (if (let* ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))
  
  (defun hydra-move-split-down (arg)
    "Move window split down."
    (interactive "p")
    (if (let* ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))
  
  (defhydra emacy-hydra-window ()
    "
  Movement^^   ^Split^         ^Switch^     ^Resize^
  -----------------------------------------------------
  _h_ Left     _v_ertical                   _q_ X left
  _j_ Down     _x_ horizontal  _f_ind files _w_ X Down
  _k_ Top      _z_ undo        _a_ce 1      _e_ X Top
  _l_ Right    _Z_ reset       _s_wap       _r_ X Right
  _F_ollow     _D_elete Other  _S_ave       max_i_mize
  _SPC_ cancel _o_nly this     _d_elete
  "
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("q" hydra-move-split-left)
    ("w" hydra-move-split-down)
    ("e" hydra-move-split-up)
    ("r" hydra-move-split-right)
    ("f" counsel-find-file)
    ("F" follow-mode)
    ("a" (ace-window 1))
    ("v" (lambda () (interactive) (split-window-right) (windmove-right)))
    ("x" (lambda () (interactive) (split-window-below) (windmove-down)))
    ("s" (ace-window 4))
    ("S" save-buffer)
    ("d" delete-window)
    ("D" (ace-window 16))
    ("o" delete-other-windows)
    ("i" ace-delete-other-windows)
    ("z" (progn (winner-undo) (setq this-command 'winner-undo)))
    ("Z" winner-redo)
    ("SPC" nil))
  
  ;;(emacy-definer "w ." 'emacy-hydra-window/body)
  
  (defhydra hydra-dired (:hint nil :color pink)
    "
  _+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
  _C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
  _D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
  _R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
  _Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
  _S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
  _r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
  _z_ compress-file  _A_ find regexp
  _Z_ compress       _Q_ repl regexp
  
  T - tag prefix
  "
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))
  
  ;; FIXME
  (general-define-key :keymaps 'dired-mode-map "." 'hydra-dired/body))
(use-package use-package-hydra :ensure t)

(elpaca-wait)

(require 'init-evil)

(provide 'init-keys)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keys.el ends here