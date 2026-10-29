;;; packages.el --- Elpaca recipes for upstream packages -*- lexical-binding: t; -*-

(defconst emacy-package-recipes
  '((aweshell :host github :repo "manateelazycat/aweshell")
    (mpvi :host github :repo "lorniu/mpvi")
    (nushell-ts-babel :host github :repo "herbertjones/nushell-ts-babel")
    (nushell-ts-mode :host github :repo "herbertjones/nushell-ts-mode")
    (ob-racket :host github :repo "hasu/emacs-ob-racket")
    (parinfer-rust-mode :host github :repo "justinbarclay/parinfer-rust-mode")
    (racket-mode :host github :repo "greghendershott/racket-mode")
    (rime :host github :repo "DogLooksGood/emacs-rime")
    (casual :host github :repo "kickingvegas/casual")
    (combobulate :host github :repo "mickeynp/combobulate"))
  "Upstream sources for packages without a usable default recipe.")

(defun emacy-package-recipe (recipe)
  "Return the configured upstream source for RECIPE, if present."
  (when-let* ((source (assq (plist-get recipe :id) emacy-package-recipes)))
    (cdr source)))

(add-hook 'elpaca-recipe-functions #'emacy-package-recipe)

(provide 'emacy-packages)
;;; packages.el ends here
