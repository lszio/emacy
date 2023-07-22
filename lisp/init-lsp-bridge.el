;;; init-lsp-bridge.el --- init lsp bridge -*- lexical-binding: t -*-

;; Author: Liszt21
;; Maintainer: liszt21
;; Version: 0.0.1
;;; Commentary:

;;; Code:

(use-package lsp-bridge
  :elpaca (lsp-bridge
           :host github
           :repo "manateelazycat/lsp-bridge"
           :files ("*" (:exclude ".git")))
  :config
  (global-lsp-bridge-mode)
  (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs))

(require 'init-snippet)

(provide 'init-lsp-bridge)

;;; init-lsp-bridge.el ends here
