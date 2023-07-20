;;; init-evil.el --- Emacs pre-initialization config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package evil
  :demand t
  :preface (setq evil-want-keybinding nil)
  :custom
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-C-i-jump t)
  (evil-undo-system 'undo-fu)
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
(use-package evil-lion :elpaca (evil-lion :host github :repo "edkolev/evil-lion" :files ("*" (:exclude ".git"))) :config (evil-lion-mode))

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

(use-package evil-goggles
  :after evil
  :demand
  :init
  (setq evil-goggles-duration 0.05)
  :config
  (push '(evil-operator-eval
          :face evil-goggles-yank-face
          :switch evil-goggles-enable-yank
          :advice evil-goggles--generic-async-advice)
        evil-goggles--commands)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-snipe
  :after evil
  :demand
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-iedit-state
  :elpaca (evil-iedit-state :repo "kassick/evil-iedit-state")
  :general
  (tyrant-def
    "s e" '(evil-iedit-state/iedit-mode :wk "iedit")
    "s q" '(evil-iedit-state/quit-iedit-mode :wk "quit iedit")))

(provide 'init-evil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-evil.el ends here