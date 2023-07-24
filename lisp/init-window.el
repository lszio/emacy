;;; init-window.el --- window -*- lexical-binding: t -*-

;; Author: Li Shuzhi
;; Maintainer: Li Shuzhi
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords

;;; Commentary:

;;; Code:

(use-package ace-window :hook (elpaca-after-init . winner-mode))

(use-package transpose-frame
  :general
  (tyrant-def
    "w [" 'transpose-frame
    "w ]" 'rotate-frame))

(provide 'init-window)

;;; init-window.el ends here
