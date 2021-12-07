(defpackage emacy
  (:use :cl)
  (:export
   :cli))

(in-package :emacy)

(defun folder-exists (path)
  (let ((p (and path (probe-file path))))
    (when (and p (not (pathname-name p))) p)))

(defparameter *home* (uiop:ensure-directory-pathname (pathname (or (uiop:getenv "EMACY") #+os-windows "~/Emacy" #-os-windows "~/.emacy"))))

(defparameter *profiles* '())

(defun shell (string &rest arguments)
  (third
    (multiple-value-list
        (uiop:run-program
            (apply #'format (cons nil (cons (format nil "~A~A" #+os-windows "powershell $OLDPWD=pwd;" #-os-windows "" string) arguments)))
            :ignore-error-status t
            :output :interactive
            :input :interactive
            :error-output :interactive))))

(defun clone-repo (remote &optional (target nil))
  (if (and target (probe-file target))
      (log:info "Repo ~A already cloned, skip..." remote)
      (progn
        (log:info "Cloning repo ~A to ~A" remote target)
        (if (zerop (shell "git clone ~A ~A" remote target))
          (log:info "Repo ~A cloned." remote)
          (log:warn "Clone ~A failed!" remote)))))

(defun link (target link)
  (log:info "Linking ~A to ~A" target link)
  (shell "~Aln -s ~A ~A -b" #+os-windows "sudo " #-os-windows "" target link))

(defun lower-case-interns (string &rest interns)
  (dolist (itn interns)
    (setf string (str:replace-all (string itn) (string-downcase (string itn)) string)))
  string)

(defun update-repo (target)
  (if (probe-file target)
      (progn
        (log:info "Updating ~A..." target)
        (when (> (shell "cd ~A; git pull --ff-only; cd $OLDPWD" target) 0)
           (log:warn "update failed")))
      (log:warn "Target ~A does not exist" target)))

(defun update-profiles-path ()
  (setf *profiles*
        (loop for profile in *profiles*
              for name = (car profile)
              for content = (cdr profile)
              for repo = (cdr (assoc "REPO" content :key #'string :test #'equal))
              for env = (cdr (assoc "ENV" content :key #'string :test #'equal))
              collect (cons name (list (cons 'user-emacs-directory (format nil "~A~A/module" *home* name))
                                       (cons 'env (list (cons (caar env) (format nil "~A~A/config" *home* name))))
                                       (cons 'repo repo))))))

(defun generate-default-profiles ()
  '(("doomemacs"
     (env ("DOOMDIR"))
     (repo
      (module . "https://github.com/hlissner/doom-emacs")
      (config . "https://github.com/Liszt21/.doom.d")))
    ("spacemacs"
     (env ("SPACEMACSDIR"))
     (repo
      (module . "https://github.com/syl20bnr/spacemacs")
      (config . "https://github.com/Liszt21/.spacemacs.d")))))

(defun save-profiles (&optional (profile-path "~/.emacs-profiles.el"))
  (with-open-file (out profile-path :direction :output :if-exists :supersede)
    (princ (lower-case-interns
            (prin1-to-string *profiles*)
            'user-emacs-directory 'env 'repo 'module 'config)
          out)))

(defun load-profiles (&optional (profile-path "~/.emacs-profiles.el"))
  (if (not (probe-file profile-path))
    (progn
      (log:warn "profile ~A does's exists, generating..." profile-path)
      (setf *profiles* (generate-default-profiles))
      (update-profiles-path)
      (save-profiles))
    (with-open-file (in profile-path)
      (setf *profiles* (read in)))))

(defun generate-profile (name module &optional config dirname)
  (cons name
        (list (cons 'user-emacs-directory (format nil "~A~A/module" *home* name))
              (cons 'repo (list (cons 'module module) (when config (cons 'config config))))
              (when (and dirname config)
                (cons 'env (list (cons dirname (format nil "~A~A/config" *home* name))))))))

(defun new-profile (name module &optional config dirname)
  (if (assoc name *profiles* :test #'equal)
      (log:warn "Profile ~A already exists" name)
      (push (generate-profile name module config dirname)
            *profiles*))
  (save-profiles))

(defun remove-profile (name)
  (if (assoc name *profiles* :test #'equal)
      (setf *profiles* (remove name *profiles* :test #'equal :key #'car))
      (log:warn "Profile ~A doesn't exists" name))
  (save-profiles))

(defun use-profile (&optional name)
  (with-open-file (out #p"~/.emacs-profile" :direction :output :if-exists :supersede)
    (princ
     (string-downcase
      (string
       (if (and name
                (some (lambda (profile) (string= (string-downcase (string (car profile))) name)) *profiles*))
           name
           (caar *profiles*))))
     out)))

(defun check ()
  (log:info "Checking...")
  (load-profiles)
  (log:info "Emacy home: ~A" *home*))

(defun get-repos (&optional (profiles *profiles*))
  (cons
   (cons "https://github.com/plexus/chemacs2" (merge-pathnames "chemacs2" *home*))
   (loop for profile in profiles
         for name = (car profile)
         for content = (cdr profile)
         for repo = (cdr (assoc 'repo content))
         for module-path = (cdr (assoc 'user-emacs-directory content))
         for config-path = (cdadr (assoc 'env content))
         for module = (cdr (assoc 'module repo))
         for config = (cdr (assoc 'config repo))
         collect (cons module module-path)
         collect (cons config config-path))))

(defun install ()
  (format t "Installing...")
  (dolist (repo (get-repos))
          (clone-repo (car repo) (cdr repo)))
  (use-profile)
  (clish:with-profile (ctx :section "emacy")
    (setf ctx (list (clish:generate-alias-define "spacemacs" "emacs --with-profile spacemacs")
                    (clish:generate-alias-define "doomemacs" "emacs --with-profile doomemacs"))))
  (link (merge-pathnames "chemacs2" *home*)
        (merge-pathnames (user-homedir-pathname) ".emacs.d")))

(defun update ()
  (log:info "Updating...")
  ;; (update-repo *home*)
  (dolist (repo (get-repos))
          (update-repo (cdr repo))))

(defun doom (&optional (command ""))
  (shell "~ADOOMDIR='~A'; ~A~A ~A"
         #+os-windows "$env:" #-os-windows "export "
         (merge-pathnames "doomemacs/config")
         (merge-pathnames "doomemacs/module/bin/doom" *home*)
         #+os-windows ".cmd" #-os-windows ""
         command))

(defun list-profiles ()
  (let ((current (str:from-file "~/.emacs-profile")))
    (format t "Profiles[~A]:~%" (length *profiles*))
    (dolist (profile *profiles*)
      (format t "  ~A[~A]~%" (car profile) (equal (car profile) current)))))

(defun info-profile (&optional name)
  (let ((profile (member (or name (caar *profiles*))
                         *profiles* :key #'car :test #'equal)))
    (print profile)))

(clish:defcli cli
  (nil #'update)
  (doom #'doom)
  (update #'update)
  (install #'install)
  ;; profile manage
  (new #'new-profile)
  (use #'use-profile)
  (info #'info-profile)
  (list #'list-profiles)
  (remove #'remove-profile)
  (:pre (lambda (&rest argv) (declare (ignorable argv)) (check))))

;;; vim: set ft=lisp lisp:
