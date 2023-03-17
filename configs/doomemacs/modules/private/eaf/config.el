(use-package! eaf)

;; apps
(use-package! eaf-browser)
(use-package! eaf-jupyter)
(use-package! eaf-browser)
(use-package! eaf-airshare)
(use-package! eaf-file-browser)
(use-package! eaf-file-manager)
(use-package! eaf-file-sender)
(use-package! eaf-music-player)
(use-package! eaf-system-monitor)
(use-package! eaf-mindmap)
(use-package! eaf-org-previewer)
(use-package! eaf-terminal)
(use-package! eaf-netease-cloud-music)
(use-package! eaf-video-player)
(use-package! eaf-image-viewer)
(use-package! eaf-pdf-viewer)
(use-package! eaf-markdown-previewer)
(use-package! eaf-camera)
;; (use-package! eaf-demo)
;; (use-package! eaf-vue-demo)

(use-package! eaf-all-the-icons)
(use-package! eaf-evil)
(use-package! eaf-interleave
  :custom
  (eaf-find-alternate-file-in-dired t)
  :diminish eaf-mode
  :bind (:map eaf-interleave-mode-map
         ("M-." . 'eaf-interleave-sync-current-note)
         ("M-p" . 'eaf-interleave-sync-previous-note)
         ("M-n" . 'eaf-interleave-sync-next-note)
         :map eaf-interleave-app-mode-map
         ("C-c M-i" . 'eaf-interleave-add-note)
         ("C-c M-o" . 'eaf-interleave-open-notes-file)
         ("C-c M-q" . 'eaf-interleave-quit))
  :config
  (add-hook 'eaf-pdf-viewer-hook 'eaf-interleave-app-mode)
  (add-hook 'eaf-browser-hook 'eaf-interleave-app-mode)
  (add-hook 'org-mode-hook 'eaf-interleave-mode)
  (setq eaf-interleave-org-notes-dir-list '("~/Notes/content"))
  (setq eaf-interleave-split-direction 'vertical)
  (setq eaf-interleave-disable-narrowing t)
  (setq eaf-interleave-split-lines 20))
(setq eaf-evil-leader-key "SPC")
