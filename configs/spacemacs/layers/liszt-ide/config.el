;; slime config
(setq inferior-lisp-program "ros")
(setq slime-lisp-implementations
      '((ros ("ros" "run"))
        (sbcl ("ros" "-L" "sbcl-bin" "run"))
        (ccl ("ros" "-L" "ccl-bin" "run"))))

;; vue config
(setq-default
 ;; web-mode
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-attr-indent-offset 2)

(spacemacs|use-package-add-hook git-gutter
  :post-config
  (progn
    ;; git-gutter
    (message "git-gutter-fringe loaded")
    ;; (setq-default fringes-outside-margins t)
    (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)))
