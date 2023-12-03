;;; init-lisp.el --- lisp -*- lexical-binding: t -*-

;; Author: Li Shuzhi
;; Maintainer: Li Shuzhi
;; Version: version

;;; Commentary:

;; 

;;; Code:

(use-package cider)

(use-package slime
  :general
  (:keymaps 'slime-mode-map
            "C-<return>" 'slime-eval-last-expression)
  :config
  (setq inferior-lisp-program "ros run"))

(use-package parinfer-rust-mode
  :hook '(emacs-lisp-mode lisp-mode)
  :config
  (electric-pair-mode -1))

(provide 'init-lisp)

;;; init-lisp.el ends here
