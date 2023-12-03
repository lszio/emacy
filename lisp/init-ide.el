;;; init-ide.el --- ide -*- lexical-binding: t -*-

;; Author: Li Shuzhi
;; Maintainer: Li Shuzhi

;;; Commentary:


;;; Code:

;; (use-package tempel
;;   :bind (("M-+" . tempel-complete)
;;          ("M-*" . tempel-insert)))

;; (use-package tempel-collection)

(use-package beacon :config (beacon-mode 1))
(use-package lentic :config (global-lentic-mode))
(use-package apheleia :config (apheleia-global-mode 1))
(use-package editorconfig :config (editorconfig-mode 1))

(use-package dap-mode
  :defer t
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-mode 1)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (require 'dap-node)

  (dap-register-debug-template
    "Node::Attach"
    (list :type "node"
          :request "attach"
          :port 9229
          :name "Node::Attach")))

(require 'init-lsp-bridge)
(require 'init-treesit)
(require 'init-web)
(require 'init-lisp)

(provide 'init-ide)
;;; init-ide.el ends here
