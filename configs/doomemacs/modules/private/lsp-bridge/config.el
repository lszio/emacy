(use-package! lsp-bridge
  :config
  (map! :map acm-mode-map
        ;; "C-j"           #'acm-select-next
        [tab]           #'acm-select-next
        [backtab]       #'acm-select-prev)
  (map! :map doom-leader-code-map
        :desc "LSP Rename"
        "r"             #'lsp-bridge-rename
        :desc "LSP Find declaration"
        "j"             #'lsp-bridge-find-def)
  (push '(lisp-mode . "cl-lsp") lsp-bridge-single-lang-server-mode-list)
  (push '(hy-mode . "hyuga") lsp-bridge-single-lang-server-mode-list)
  (push 'lisp-mode lsp-bridge-default-mode-hooks)
  (push 'hy-mode lsp-bridge-default-mode-hooks)
  (push 'lisp-mode lsp-bridge-formatting-indent-alist)
  (push 'hy-mode lsp-bridge-formatting-indent-alist)
  (require 'yasnippet)
  (yas-global-mode 1)
  (global-lsp-bridge-mode))
