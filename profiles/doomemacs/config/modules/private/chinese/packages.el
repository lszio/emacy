;; FIXME

;; (defun initialize-rime ()
;;   (rime-activate "post-build")
;;   (let* ((installation-file (concat rime-user-data-dir "/installation.yaml"))
;;          (installation-data (with-temp-buffer
;;                               (insert-file-contents installation-file)
;;                               (yaml-parse-string (buffer-string)))))
;;     installation-data)
;;   (rime-deploy)
;;   (rime-sync))

(defun get-sync-dir (sync-directory) (or sync-directory (getenv "RIME_SYNC_DIR") "~/Sync/System/Rime"))

(defun sync-data (backup-path target-path)
  (when (and (file-directory-p backup-path)
             (not (file-directory-p target-path)))
    (copy-directory backup-path target-path t t t)))

(defun init-rime-from-sync (&optional sync-dir)
  (require 'rime)
  (let* ((sync-path (get-sync-dir sync-dir))
         (emacs-path (concat sync-dir "/emacs"))
         (fcitx5-path (concat sync-dir "/fcitx5"))
         (weasel-path (concat sync-dir "/weasel")))
    (sync-data emacs-path rime-user-data-dir)
    (when (and IS-LINUX (file-directory-p fcitx5-path))
      (sync-data fcitx5-path "~/.local/share/fcitx5/rime"))
    (when (and IS-WINDOWS (file-directory-p weasel-path))
      (sync-data fcitx5-path "~/AppData/Roaming/Rime"))))

;; (package! sis)
(package! rime
  :recipe (:post-build (init-rime-from-sync)))
