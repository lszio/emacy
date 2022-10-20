(package! lsp-bridge
  :recipe (:host github
           :repo "manateelazycat/lsp-bridge"
           :files ("*")))

;; (package! posframe)
;; (package! yasnippet)
(package! lsp-mode :disable t :ignore t)
(package! company :disable t :ignore t)
