(defun dump-custom-langserver (app-dir)
  (with-temp-file (concat app-dir "/langserver/cl-lsp.json")
    (insert (json-serialize '((name . "cl-lsp")
                              (languageId . "common-lisp")
                              (command . ["cl-lsp"])
                              (settings . nil)))))
  (with-temp-file (concat app-dir "/langserver/hyuga.json")
    (insert (json-serialize '((name . "hyuga")
                              (languageId . "hy")
                              (command . ["hyuga"])
                              (settings . nil))))))

(package! lsp-bridge
  :recipe (:host github
           :repo "manateelazycat/lsp-bridge"
           :files ("*")
           :pre-build
           ;; (dump-custom-langserver (concat straight-base-dir "straight/" straight-build-dir "/lsp-bridge"))
           (dump-custom-langserver (concat straight-base-dir "straight/" "repos" "/lsp-bridge"))))

(package! posframe)
(package! yasnippet)
(package! lsp-mode :disable t :ignore t)
(package! company :disable t :ignore t)
(package! corfu :disable t :ignore t)
