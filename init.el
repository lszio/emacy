;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'cl-lib)

(defvar emacy-directory "~/Emacy")
(defvar user-local-directory "~/.local")

(display-time-mode)
(toggle-frame-maximized)
(global-auto-revert-mode)
(global-display-line-numbers-mode)

(electric-pair-mode)
(toggle-word-wrap)
(global-word-wrap-whitespace-mode 1)
(tab-bar-mode -1)

;; 将lisp目录放到加载路径的前面以加快启动速度
(add-to-list 'load-path (file-name-as-directory (locate-user-emacs-file "lisp")))

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
      (add-to-list 'load-path dir)
      (normal-top-level-add-subdirs-to-load-path)))
;;(when windows? (add-subdirs-to-load-path elpaca-repos-directory))

(require 'init-elpaca)
(require 'init-keys)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
