#!/usr/bin/env -S emacs --script
(require 'cl-lib)

;;; Code:
(defvar *home* (expand-file-name (or (getenv "EMACY_HOME") "~/.emacy")))
(defvar *profile-envs* '(("spacemacs" . "SPACEMACSDIR") ("doomemacs" . "DOOMDIR") ("lemacs" . "LEMACSDIR")))

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (eq system-type 'gnu/linux))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

(defun detect-profiles (home)
  "Detect profiles (HOME)."
  (cl-loop for profile-name in (directory-files (concat home "/profiles") nil "[^(chemacs2|\.)].+")
           for env-name = (cdr (assoc profile-name *profile-envs*))
           for env-data = (if env-name `(env (,env-name . ,(concat home "/configs/"  profile-name))))
           for profile-dir = (concat home "/profiles/" profile-name)
           for bin-dir = (concat profile-dir "/bin")
           for bin-files = (if (file-exists-p bin-dir) (directory-files bin-dir nil "[^\.]+"))
           for bin-data = `(bins . ,bin-files)
           for server = `(server-name . profile-name)
           for profile-data = `((user-emacs-directory . ,profile-dir) ,server ,env-data ,bin-data)
           collect (cons profile-name profile-data)))

(defvar *profiles* (detect-profiles *home*))

(defun save-to (file value)
  "Save VALUE to FILE."
  (with-temp-file file (prin1 value (current-buffer))))

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
  (save-to "~/.emacs-profiles.el" (detect-profiles *home*))
  (pp *profiles*)
  (link-files))

(defun use (&optional profile)
  "Use PROFILE."
  (defvar current (symbol-name (read-from "~/.emacs-profile")))
  (defvar profiles (mapcar 'car *profiles*))
  (princ (format "current profile: %s\nprofiles: " current))
  (pp profiles)
  (if (member profile profiles)
      (princ (format "Change to %s" (with-temp-file "~/.emacs-profile" (princ profile (current-buffer)))))
      (princ (format "profile %s not in ~/.emacs-profiles.el\n" profile))))

(defun dispatch-args (args fn-alist docs)
  "Dispatch ARGS to FN-ALIST DOCS."
  (let ((fn (cdr (assoc (car args) fn-alist))))
    (if fn
        (apply fn (cdr args))
        (princ docs))))

(defun link-files ()
  "Make link to all executable."
  (print (format "Link %s => %s" "~/.emacs.d" (file-name-concat *home* "tools" "chemacs2")))
  (make-symbolic-link (file-name-concat *home* "tools" "chemacs2") "~/.emacs.d" t)
  (cl-loop for path in (directory-files (file-name-concat *home* "scripts") t directory-files-no-dot-files-regexp)
           for name = (file-name-base path)
           when (file-executable-p path)
           do (make-symbolic-link path (format "~/.local/bin/%s" name) t))
  (cl-loop for profile in *profiles*
           for profile-dir = (cdr (assoc 'user-emacs-directory (cdr profile)))
           for bins = (cdr (assoc 'bins (cdr profile)))
           do (cl-loop for bin-name in bins
                       for linkname = (file-name-concat "~/.local/bin/" bin-name)
                       for target = (file-name-concat profile-dir "bin" bin-name)
                       do (print (format "Link %s => %s" linkname target))
                       do (make-symbolic-link target linkname t))))

(defvar *hook-templates* '((powershell . ((hook-format . "# Emacy powershell hook
function __emacy-hook(){
%s
}
__emacy-hook
unset -f __emacy-hook")
                                          (env-format . "    env:%s=%s")))
                           (bash . ((hook-format . "# Emacy bash hook
function __emacy-hook(){
    # envs
%s
    # alias
}
%s
__emacy-hook
unset -f __emacy-hook")
                                    (env-format . "    export %s=%s")
                                    ;; FIXME: alias does't work
                                    (alias-format . "alias %s='emacs --with-profile %s'")))))

(defun get-format-string (name type &optional default)
  "Get format-string with(NAME TYPE DEFAULT)."
  (or (cdr (assoc name (cdr (assoc (intern (or type "bash"))
                                   *hook-templates*))))
      default))

(defsubst string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR."
  (mapconcat #'identity strings separator))

(defun hook (&optional type)
  "Generate hook for shell TYPE."
  (let* ((template (cdr (assoc (intern type) *hook-templates*)))
         (hook-format (get-format-string 'hook-format type))
         (env-format (get-format-string 'env-format type))
         (alias-format (get-format-string 'alias-format type))
         (envs (string-join (cl-loop for profile in *profiles*
                                     for env = (cadr (assoc 'env profile))
                                     collect (format env-format (car env) (cdr env)))
                            "\n"))
         (aliases (string-join (cl-loop for profile in *profiles*
                                        for name = (car profile)
                                        collect (format alias-format name name))
                               "\n"))
         (body (format hook-format envs aliases)))
    (princ body)))

(defun main (&rest args)
  "Main ARGS."
  (dispatch-args args '(("init" . init)
                        ("hook" . hook)
                        ("use" . use))
                 "emacy: my emacs's script
  commands:
    init         : initialize emacs config
    hook         : hook
    use (profile): use profile"))

(apply 'main command-line-args-left)

(provide 'emacy)
;;; emacy.el ends here
