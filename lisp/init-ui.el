
(defun fonts-installed (&rest font-list) (reverse (cl-intersection font-list (font-family-list) :test #'equal)))

(use-package fontaine
  :ensure t
  :when (display-graphic-p)
  :config
  (setq fontaine-latest-state-file (locate-user-emacs-file "etc/fontaine-latest-state.eld"))
  (setq fontaine-presets
    '((regular
       :default-height 140
       :default-weight regular
       :fixed-pitch-height 1.0
       :variable-pitch-height 1.0)
      (large
       :default-height 180
       :default-weight normal
       :fixed-pitch-height 1.0
       :variable-pitch-height 1.05)
      (t
       :default-family "Fira Code"
       :fixed-pitch-family "Fira Code"
       :variable-pitch-family "Fira Code"
       :italic-family "Fira Code"
       :variable-pitch-weight normal
       :bold-weight normal
       :italic-slant italic
       :line-spacing 0.1)))
  ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-set-preset 'regular)

  ;; set emoji font
  (set-fontset-font t (if (version< emacs-version "28.1") '(#x1f300 . #x1fad0) 'emoji)
    (car (fonts-installed
          "Noto Emoji"
          "Symbola"
          "Apple Color Emoji"
          "Noto Color Emoji"
          "Segoe UI Emoji")))
  ;; set Chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :family
                (car (fonts-installed
                       "LXGW Wenkai"
                       "霞鹜文楷"
                       "Sarasa Gothic SC"
                       "更纱黑体 SC")))))

  ;; set Chinese font scale
  (setq face-font-rescale-alist `(
                                  ("Symbola"             . 1.3)
                                  ("Microsoft YaHei"     . 1.2)
                                  ("WenQuanYi Zen Hei"   . 1.2)
                                  ("Sarasa Mono SC Nerd" . 1.2)
                                  ("PingFang SC"         . 1.16)
                                  ("Lantinghei SC"       . 1.16)
                                  ("Kaiti SC"            . 1.16)
                                  ("Yuanti SC"           . 1.16)
                                  ("Apple Color Emoji"   . 0.91))))
;; TODO
(use-package fontify-face)

(require 'init-theme)

(provide 'init-ui)