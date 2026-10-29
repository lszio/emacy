;;; envs.el --- managed external runtimes and LSP RPC -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst emacy-environment-root (expand-file-name "env" emacy-config-root)
  "Directory containing external runtime environments.")
(defconst emacy-python-venv-root
  (expand-file-name "python/.venv" emacy-environment-root))
(defconst emacy-python-venv-bin-directory
  (expand-file-name (or (and (boundp 'emacy-python-venv-bin)
                             emacy-python-venv-bin)
                        "bin")
                    emacy-python-venv-root))
(defconst emacy-node-modules-bin-directory
  (expand-file-name "node/node_modules/.bin" emacy-environment-root))

(defun emacy-env--add-exec-path (directory)
  "Put existing DIRECTORY at the front of `exec-path' and `PATH'."
  (when (file-directory-p directory)
    (setq exec-path (cons directory (delete directory exec-path)))
    (setenv "PATH" (mapconcat #'identity exec-path path-separator))))

(defun emacy-env--executable (directory name)
  "Find NAME in DIRECTORY, falling back to the current `exec-path'."
  (let ((candidate (expand-file-name name directory)))
    (or (and (file-executable-p candidate) candidate)
        (executable-find name))))

(defun emacy-env-configure-eglot ()
  "Register managed Python and TypeScript language servers with Eglot.
Both servers communicate with Emacs using the Language Server Protocol's
JSON-RPC transport over stdio."
  (let ((python-server (emacy-env--executable emacy-python-venv-bin-directory "pylsp"))
        (typescript-server
         (emacy-env--executable emacy-node-modules-bin-directory
                                "typescript-language-server")))
    (when python-server
      (dolist (mode '(python-mode python-ts-mode))
        (setf (alist-get mode eglot-server-programs) (list python-server))))
    (when typescript-server
      (dolist (mode '(js-mode js2-mode js-ts-mode typescript-mode
                      typescript-ts-mode tsx-ts-mode))
        (setf (alist-get mode eglot-server-programs)
              (list typescript-server "--stdio"))))))

(defun emacy-env-initialize ()
  "Add existing managed runtime directories to `exec-path' and `PATH'."
  (dolist (directory (list emacy-python-venv-bin-directory
                           emacy-node-modules-bin-directory
                           (expand-file-name "~/.bun/bin")
                           (expand-file-name "~/.local/bin")))
    (emacy-env--add-exec-path directory)))

(emacy-env-initialize)

(with-eval-after-load 'eglot
  (emacy-env-configure-eglot))

(provide 'emacy-envs)
;;; envs.el ends here
