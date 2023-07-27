;;; init-ide.el --- ide -*- lexical-binding: t -*-

;; Author: Li Shuzhi
;; Maintainer: Li Shuzhi
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;;; Commentary:


;;; Code:

(use-package apheleia :config (apheleia-global-mode +1))
(use-package editorconfig :config (editorconfig-mode 1))
(use-package dap-mode
  :defer t
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-mode 1)
  ;; (dap-ui-mode 1)
  ;; (dap-ui-controls-mode 1)
  ;; (dap-tooltip-mode 1)
  ;; (tooltip-mode 1)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (require 'dap-node)

  (dap-register-debug-template
    "Node::Attach"
    (list :type "node"
          :request "attach"
          :port 9229
          :name "Node::Attach")))

(require 'init-dev)

(provide 'init-ide)

;;; init-ide.el ends here
