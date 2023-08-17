;;; init-theme.el --- theme -*- lexical-binding: t -*-

;; Author: Li Shuzhi
;; Maintainer: Li Shuzhi

;;; Commentary:

;;; Code:

(use-package ef-themes
  :ensure t
  :bind ("C-c t" . ef-themes-toggle)
  :init
  ;; set two specific themes and switch between them
  (setq ef-themes-to-toggle '(ef-summer ef-winter))
  ;; set org headings and function syntax
  (setq ef-themes-headings ; read the manual's entry or the doc string
      '((0 . (variable-pitch light 1.2))
        (1 . (variable-pitch light 1.1))
        (2 . (variable-pitch regular 1.0))
        (3 . (variable-pitch regular 1.0))
        (4 . (variable-pitch regular 1.0))
        (5 . (variable-pitch 1.0)) ; absence of weight means `bold'
        (6 . (variable-pitch 1.0))
        (7 . (variable-pitch 1.0))
        (t . (variable-pitch 1.0))))
  (setq ef-themes-region '(intense no-extend neutral))
  ;; Disable all other themes to avoid awkward blending:
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-load-random 'dark)

  :config
  ;; auto change theme, aligning with system themes.
  (defun my/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (if (display-graphic-p) (ef-themes-load-random 'light) (ef-themes-load-random 'dark)))
      ('dark (ef-themes-load-random 'dark))))

  (when macos? (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)))

(provide 'init-theme)

;;; init-theme.el ends here
