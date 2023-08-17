;;; init-web.el --- web -*- lexical-binding: t -*-

;; Author: Li Shuzhi
;; Maintainer: Li Shuzhi

;;; Commentary:

;;; Code:
(use-package css-mode :elpaca nil :init (setq css-indent-offset 2))
;; (use-package scss-mode :init (setq scss-compile-at-save nil))
;; (use-package less-css-mode)
(use-package json-mode)

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode)))

(use-package prettier
  :diminish
  :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
  :init (setq prettier-pre-warm 'none))

(use-package typescript-mode :mode ("\\.ts[x]\\'" . typescript-mode))

(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Adds node_modules/.bin directory to `exec_path'
(use-package add-node-modules-path :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package restclient-test
    :diminish
    :hook (restclient-mode . restclient-test-mode)))

(provide 'init-web)

;;; init-web.el ends here
