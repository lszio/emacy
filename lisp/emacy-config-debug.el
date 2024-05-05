;;; init-dev.el --- development settings for init file  -*- lexical-binding: t; -*-
;;; Code:
(defcustom emacy-library-directory (concat user-emacs-directory "libs/") "Libraries for Emacs.")

(add-to-list 'load-path (concat emacy-library-directory "org-auto-tangle/"))

(setq-local org-confirm-babel-evaluate nil)

(require 'org-auto-tangle)
(org-auto-tangle-mode)
(add-hook 'org-auto-tangle-before-tangle-hook #'check-parens)
(add-hook 'org-auto-tangle-after-tangle-hook
        (lambda () (load-file (concat user-emacs-directory "init.el"))
            (when-let ((forms (elpaca-q<-forms (car elpaca--queues))))
            (eval `(progn ,@(apply #'append (mapcar #'cdr (reverse forms)))) t))))

(eldoc-mode)
(provide 'emacy-config-debug)
;; init-dev.el ends here