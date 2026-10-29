;;; early-init.el --- startup and runtime detection -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Keep the byte-compilation cache with the other runtime state instead
;; of the configuration root.  `startup-redirect-eln-cache' is meant to
;; be called from the early init file.  Neomacs has no native compiler
;; (`native-comp-eln-load-path' is unbound), so skip it there.
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache "var/eln-cache"))

(defconst windows? (memq system-type '(cygwin windows-nt ms-dos))
  "Non-nil when running on Windows.")
(defconst linux? (memq system-type '(gnu/linux gnu gnu/kfreebsd))
  "Non-nil when running on Linux.")
(defconst macos? (eq system-type 'darwin)
  "Non-nil when running on macOS.")
(defconst bsd? (memq system-type '(darwin berkeley-unix gnu/kfreebsd))
  "Non-nil when running on BSD.")
(defconst wsl?
  (and linux?
       (string-match-p "microsoft"
                       (downcase (or (and (boundp 'operating-system-release)
                                          operating-system-release)
                                     ""))))
  "Non-nil when running in WSL.")
(defconst neomacs? (fboundp 'neovm--internal-panic)
  "Non-nil when running in Neomacs.")

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      frame-inhibit-implied-resize t
      inhibit-splash-screen t
      inhibit-startup-screen t
      use-file-dialog nil
      comp-deferred-compilation nil)

(unless noninteractive
  (display-time-mode 1)
  (global-auto-revert-mode 1)
  (global-display-line-numbers-mode 1)
  (toggle-word-wrap)
  (global-word-wrap-whitespace-mode 1)
  (electric-pair-mode 1)
  (when (display-graphic-p)
    (toggle-frame-maximized)))

(tab-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
