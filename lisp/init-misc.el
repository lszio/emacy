;;; init-misc.el --- misc -*- lexical-binding: t -*-

;; Author: Li Shuzhi
;; Maintainer: Li Shuzhi

;;; Commentary:

;;; Code:
(use-package no-littering
  :demand
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")
        whisper-install-directory (no-littering-expand-var-file-name "whisper")
        parinfer-rust-library-directory (no-littering-expand-var-file-name "parinfer-rust"))
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

(use-package helpful
  :after evil
  :init
  (setq evil-lookup-func #'helpful-at-point)
  :bind
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable))

(use-package exec-path-from-shell
  :when (not windows?)
  :config
  (exec-path-from-shell-initialize))

(use-package whisper :elpaca (whisper :host github :repo "natrys/whisper.el")
  :general
  (tyrant-def
    "aw" (cons "whisper" (make-sparse-keymap))
    "awr" 'whisper-run
    "awf" 'whisper-file)
  :config
  (setq whisper-model "base"
        whisper-language "cn"
        whisper-translate nil))

(provide 'init-misc)
;;; init-misc.el ends here
