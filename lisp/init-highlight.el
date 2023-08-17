;;; init-highlight.el --- highlight -*- lexical-binding: t -*-

;; Author: Li Shuzhi
;; Maintainer: Li Shuzhi

;;; Commentary:

;;; Code:

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'fill))

(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-highlight)

;;; init-highlight.el ends here
