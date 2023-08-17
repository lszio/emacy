;;; init-icon.el --- icon -*- lexical-binding: t -*-

;; Author: Li Shuzhi
;; Maintainer: Li Shuzhi

;;; Commentary:

;;; Code:
(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

;; (use-package all-the-icons
;;   :if (display-graphic-p)
;;   :demand)

;; (use-package all-the-icons-completion
;;   :if (display-graphic-p)
;;   :after (marginalia all-the-icons)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init (all-the-icons-completion-mode))

;; (use-package treemacs-all-the-icons :after (treemacs all-the-icons))

(provide 'init-icon)

;;; init-icon.el ends here
