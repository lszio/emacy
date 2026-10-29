;;; macos.el --- macOS-specific runtime paths -*- lexical-binding: t; -*-

(defconst emacy-python-venv-bin "bin"
  "Directory containing executables in the managed Python virtualenv.")

(use-package exec-path-from-shell
  :ensure t
  :if (display-graphic-p)
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
  :config
  (exec-path-from-shell-initialize)
  (when (fboundp 'emacy-env-initialize)
    (emacy-env-initialize)))

(provide 'emacy-platform-macos)
;;; macos.el ends here
