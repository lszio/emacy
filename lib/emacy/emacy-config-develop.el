;;; emacy-config-develop.el --- development settings for emacs config -*- lexical-binding: t; -*-
;;; Code:
(defcustom emacy-library-directory (concat user-emacs-directory "lib/") "Libraries for Emacs.")

(add-to-list 'load-path (concat emacy-library-directory "org-auto-tangle/"))

(setq-local org-confirm-babel-evaluate nil)

(require 'org-auto-tangle)
(org-auto-tangle-mode)
(add-hook 'org-auto-tangle-before-tangle-hook #'check-parens)
(add-hook 'org-auto-tangle-after-tangle-hook (lambda () (load-file (concat user-emacs-directory "init.el"))))

(eldoc-mode)
(provide 'emacy-config-develop)
;; emacy-config-develop.el ends here