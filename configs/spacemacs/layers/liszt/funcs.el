(defun liszt/install-depedencies ()
  (interactive)
  (setq package-manager (eval `(or ,@(mapcar (lambda (cmd) `(when (zerop (shell-command ,cmd)) ,cmd)) '("yay" "pacamn" "apt" "yum" "guix" "nix" "scoop")))))
  ;; layer liszt

  ;; layer liszt-tool
  ;; +rime
  ;; (case package-manager
  ;;   ("scoop"
  ;;    (print "Install librime")))
     ;; (shell-command "scoop bucket add wsw0108 https://github.com/wsw0108/scoop-bucket.git")
     ;; (shell-command "scoop install gcc librime")))
  (when (equal package-manager "scoop")
    (shell-command "scoop bucket add wsw0108 https://github.com/wsw0108/scoop-bucket.git")
    (shell-command "scoop install gcc librime")))
