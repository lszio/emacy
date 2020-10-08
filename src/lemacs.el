;;; package --- Summary
;;; Code:
(message "Lemacs")

(defvar lemacs-profiles-path "~/.emacs-profiles.el")
(defvar lemacs-default-profile-path "~/.emacs-profile")

(when (not (file-exists-p lemacs-profiles-path))
  (with-temp-file lemacs-profiles-path
    (insert "((\"default\" . ((user-emacs-directory . \"~/doomemacs\")))\n (\"doomemacs\" . ((user-emacs-directory . \"~/doomemas\")))\n (\"spacemacs\" . ((user-emacs-directory . \"~/spacemacs\")))\n (\"origin\" . ((user-emacs-directory . \"~/.emacs.d\"))))")))

(defvar lemacs-emacs-profiles
  (with-temp-buffer
    (insert-file-contents lemacs-profiles-path)
    (goto-char (point-min))
    (read (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lemacs-detect-default-profile ()
  (if (file-exists-p lemacs-default-profile-path)
      (with-temp-buffer
        (insert-file-contents lemacs-default-profile-path)
        (goto-char (point-min))
        ;; (buffer-string))
        (symbol-name (read (current-buffer)) ))
    "default"))

(defun lemacs-load-straight ()
  (defvar bootstrap-version)
  (let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lemacs-get-emacs-profile (profile)
  (cdr (assoc profile lemacs-emacs-profiles)))

(defun lemacs-emacs-profile-key (key &optional default)
  (alist-get key (lemacs-get-emacs-profile lemacs-current-emacs-profile)
             default))

(defun lemacs-load-profile (profile)
  (when (not (lemacs-get-emacs-profile profile))
    (error "No profile `%s' in %s" profile lemacs-profiles-path))
  (setq lemacs-current-emacs-profile profile)
  (let* ((emacs-directory (file-name-as-directory
                           (lemacs-emacs-profile-key 'user-emacs-directory)))
         (init-file       (expand-file-name "init.el" emacs-directory))
         (custom-file-    (lemacs-emacs-profile-key 'custom-file init-file))
         (server-name-    (lemacs-emacs-profile-key 'server-name)))
    (setq user-emacs-directory emacs-directory)

    ;; Allow multiple profiles to each run their server
    ;; use `emacsclient -s profile_name' to connect
    (when server-name-
      (setq server-name server-name-))

    ;; Set environment variables, these are visible to init-file with getenv
    (mapcar (lambda (env)
              (setenv (car env) (cdr env)))
            (lemacs-emacs-profile-key 'env))

    (when (lemacs-emacs-profile-key 'straight-p)
      (lemacs-load-straight))

    ;; Start the actual initialization
    (load init-file)

    ;; Prevent customize from changing ~/.emacs (this file), but if init.el has
    ;; set a value for custom-file then don't touch it.
    (when (not custom-file)
      (setq custom-file custom-file-)
      (unless (equal custom-file init-file)
        (load custom-file)))))

(defun lemacs-check-command-line-args (args)
  (if args
      ;; Handle either `--with-profile profilename' or
      ;; `--with-profile=profilename'
      (let ((s (split-string (car args) "=")))
        (cond ((equal (car args) "--with-profile")
               ;; This is just a no-op so Emacs knows --with-profile
               ;; is a valid option. If we wait for
               ;; command-switch-alist to be processed then
               ;; after-init-hook has already run.
               (add-to-list 'command-switch-alist
                            '("--with-profile" .
                              (lambda (_) (pop command-line-args-left))))
               ;; Load the profile
               (lemacs-load-profile (cadr args)))

              ;; Similar handling for `--with-profile=profilename'
              ((equal (car s) "--with-profile")
               (add-to-list 'command-switch-alist `(,(car args) . (lambda (_))))
               (lemacs-load-profile (mapconcat 'identity (cdr s) "=")))

              (t (lemacs-check-command-line-args (cdr args)))))

    ;; If no profile given, load the "default" profile
    (lemacs-load-profile (lemacs-detect-default-profile))))

;; Check for a --with-profile flag and honor it; otherwise load the
;; default profile.
(lemacs-check-command-line-args command-line-args)

(provide '.emacs)
;;; Commentary:
;;; lemacs.el ends here
