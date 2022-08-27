#!/usr/bin/env -S emacs --script
(require 'cl-lib)

;;; Code:
(defvar *home* (or (getenv "EMACY_HOME") "~/Emacy"))
(defvar home (or (getenv "EMACY_HOME") "~/Emacy"))

(defun generate-profiles (home)
  "Geenrate profiles (HOME)."
  (let* ((envs '(("spacemacs" . "SPACEMACSDIR") ("doomemacs" . "DOOMDIR") ("lemacs" . "LEMACSDIR"))))
    (cl-loop for module in (directory-files (concat home "/modules") nil "[^(chemacs2|\.)].+")
             for env-name = (cdr (assoc module envs))
             for env = (if env-name `(env (,env-name . ,(concat home "/configs/"  module))))
             for module-dir = (concat home "/modules/" module)
             for server = `(server-name . module)
             for profile = `((user-emacs-directory . ,module-dir) ,server ,env)
             collect (cons module profile))))

(file-name-base "/asdg/sadg")

(defun save-to (file value)
  "Save VALUE to FILE."
  (with-temp-file file
    (prin1 value (current-buffer))))

(defun read-from (file)
  "Read from FILE."
  (if (file-exists-p file)
      (with-temp-buffer
         (insert-file-contents file)
         (cl-assert (eq (point) (point-min)))
         (read (current-buffer)))))

(defun read-as-string (file)
  "Read from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (cl-assert (eq (point) (point-min)))
    (buffer-string)))

(defun init ()
  "Initialize emacy."
  (print "init")
  (defvar profiles (generate-profiles *home*))
  (save-to "~/.emacs-profiles.el" profiles)
  (pp profiles)
  (link-executable))

(defun use (&optional profile)
  "Use PROFILE."
  (defvar current (symbol-name (read-from "~/.emacs-profile")))
  (defvar profiles (mapcar 'car (read-from "~/.emacs-profiles.el")))
  (princ (format "current profile: %s\nprofiles: " current))
  (princ profiles)
  (princ "\n")
  (if (member profile profiles)
      ;; (save-to "~/.emacs-profile")
      (with-temp-file "~/.emacs-profile" (princ profile (current-buffer)))
      (princ (format "profile %s not in ~/.emacs-profiles.el\n" profile))))

(defun dispatch-args (args fn-alist docs)
  "Dispatch ARGS to FN-ALIST DOCS."
  (let ((fn (cdr (assoc (car args) fn-alist))))
    (if fn
        (apply fn (cdr args))
        (princ docs))))

;; (directory-files-recursively "~/Emacy/modules" "^bin$" t)
(file-expand-wildcards "~/Emacy/modules/*/bin/*")

(defun link-executable ()
  "Make link to all executable."
  (cl-loop for file in (file-expand-wildcards (concat *home* "/scripts/*.el") nil)
           do (print file)
           for target = (concat "~/.local/bin/" (file-name-base file))
           when (or (file-symlink-p target)
                    (and (file-exists-p target) (not (file-directory-p target))
                         (y-or-n-p (format "remove exist file: %s?" target))))
           do (progn (shell-command (format "rm %s" target) standard-output)
                     (princ (format "remove exists target: %s\n" target)))
           when (and (file-executable-p file) (zerop (shell-command (format "ln -s %s %s" file target) nil)))
           do (princ (format "Link %s => %s\n" file target)))
  (cl-loop for file in (file-expand-wildcards (concat *home* "/modules/*/bin/*"))
           for target = (concat "~/.local/bin/" (file-name-nondirectory file))
           when (or (file-symlink-p target)
                    (and (file-exists-p target) (not (file-directory-p target))
                         (y-or-n-p (format "remove exist file: %s?" target))))
           do (progn (shell-command (format "rm %s" target) standard-output)
                     (princ (format "remove exists target: %s\n" target)))
           when (and (file-executable-p file) (zerop (shell-command (format "ln -s %s %s" file target) nil)))
           do (princ (format "Link %s => %s\n" file target))))

(defun help ()
  "Help."
  (pp '((init)
        (use))))


(defun main (&rest args)
  "Main ARGS."
  (dispatch-args args '(("init" . init)
                        ("use" . use))
                 "emacy: my emacs's script
  commands:
    init         : initialize emacs config
    use (profile): use profile"))

(apply 'main command-line-args-left)

(provide 'emacy)
;;; emacy.el ends here
