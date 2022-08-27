#!emacs --script
(require 'cl-lib)

;;; Code:
(defvar *home* (or (getenv "EMACY_HOME") "~/Emacy"))

(defun generate-profiles (home)
  "Geenrate profiles (HOME)."
  (let* ((envs '(("spacemacs" . "SPACEMACSDIR") ("doomemacs" . "DOOMDIR") ("lemacs" . "LEMACSDIR"))))
    (cl-loop for module in (directory-files (concat home "/modules") nil "^[^(chemacs2|.+)].*$")
             for env-name = (cdr (assoc module envs))
             for env = (if env-name `(env (,env-name . ,(concat "/configs/"  module))))
             for module-dir = (concat home "/modules/" module)
             for profile = `((user-emacs-directory . ,module-dir) ,env)
             collect (cons module profile))))

(defun save-to (file value)
  "Save VALUE to FILE."
  (with-temp-file file
    (prin1 value (current-buffer))))

(defun read-from (file)
  "Read from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

(defun read-as-string (file)
  "Read from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (cl-assert (eq (point) (point-min)))
    (buffer-string)))

(defun init ()
  "Initialize emacy."
  (print "init")
  (save-to "~/.emacs-profiles.el" (generate-profiles *home*)))

(defun use (&optional profile)
  "Use PROFILE."
  (defvar current (symbol-name (read-from "~/.emacs-profile")))
  (defvar profiles (mapcar 'car (read-from "~/.emacs-profiles.el")))
  (princ (format "current profile: %s\nprofiles: " current))
  (princ profiles)
  (princ "\n")
  (if (member profile profiles)
      (save-to "~/.emacs-profile" profile)
      (princ (format "profile %s not in ~/.emacs-profiles.el\n" profile))))

(defun dispatch-args (args fn-alist)
  "Dispatch ARGS to FN-ALIST."
  (let ((fn (cdr (assoc (car args) fn-alist))))
    (if fn (apply fn (cdr args))
      (print "no fn matched"))))

(defun main (&rest args)
  "Main ARGS."
  (dispatch-args args '(("init" . init)
                        ("use" . use))))

(apply 'main (nthcdr 3 command-line-args))

(provide 'emacy)
;;; emacy.el ends here
