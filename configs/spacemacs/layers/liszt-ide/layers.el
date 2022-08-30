(configuration-layer/declare-layers
 '(
    ;; -- languages --
    python
    ;; -- frontend --
    prettier
    (html :variables
          web-fmt-tool 'prettier)
    (javascript :variables
                js2-basic-offset 2
                js-indent-level 2)
    (typescript :variables
                typescript-linter 'eslint
                typescript-fmt-tool 'prettier
                typescript-indent-level 2)
    (json :variables
          json-fmt-tool 'prettier)
    (vue :variables
         vue-backend 'lsp)
    (node :variables
          node-add-modules-path t)
    react
    yaml
    (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
    (julia :variables julia-backend 'lsp)
    ;; -- lisp --
    parinfer
    common-lisp
    emacs-lisp
    scheme
    haskell
    (clojure :variables
             clojure-backend 'cider               ;; use cider and disable lsp
             clojure-enable-linters 'clj-kondo    ;; clj-kondo included in lsp
             cider-repl-display-help-banner nil      ;; disable help banner
             cider-pprint-fn 'fipp                   ;; fast pretty printing
             clojure-indent-style 'align-arguments
             clojure-align-forms-automatically t
             clojure-toplevel-inside-comment-form t  ;; evaluate expressions in comment as top level
             cider-result-overlay-position 'at-point ;; results shown right after expression
             cider-overlays-use-font-lock t
             cider-repl-buffer-size-limit 100)        ;; limit lines shown in REPL buffer
    ;; -- complate --
    (auto-completion :variables auto-completion-use-company-box t)
    lsp
    ;; ---- tools ----
    (version-control :variables
                     version-control-diff-side 'left)
    git
    markdown
    wakatime
    plantuml
    graphviz
    ;; spell-checking
    syntax-checking
    command-log
    (ranger :variables
            ranger-show-preview t
            ranger-show-hidden t
            ranger-cleanup-eagerly t
            ranger-cleanup-on-disable t
            ranger-ignored-extensions '("mkv" "flv" "iso" "mp4"))))
