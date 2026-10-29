;;; init.el --- Emacy configuration loader -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar emacy-config-root user-emacs-directory)
(defvar emacy-lisp-directory (expand-file-name "lisp" emacy-config-root))
(defvar emacy-var-directory (expand-file-name "var" emacy-config-root)
  "Single home for generated artifacts and runtime state.")
(add-to-list 'load-path emacy-lisp-directory)
;; Native modules built by `make vterm-module' (e.g. vterm-module.so)
;; live under var/; `require' finds modules via `load-path'.
(add-to-list 'load-path (expand-file-name "modules" emacy-var-directory))
(load (expand-file-name "packages.el" emacy-lisp-directory) nil 'nomessage)

(let ((platform (cond (windows? "windows")
                      (macos? "macos")
                      (linux? "linux")
                      (t nil))))
  (when platform
    (load (expand-file-name (concat "platform/" platform ".el")
                            emacy-lisp-directory)
          nil t 'nomessage)))

(load (expand-file-name "envs.el" emacy-lisp-directory) nil 'nomessage)
(load (expand-file-name "base.el" emacy-lisp-directory) nil 'nomessage)

(provide 'emacy-init)
;;; init.el ends here
