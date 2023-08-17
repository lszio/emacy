;;; init-complete.el --- complete -*- lexical-binding: t -*-

;; Author: Li Shuzhi
;; Maintainer: Li Shuzhi
;; Version: version
;;; Commentary:

;;; Code:

(message "Hello World!")

(use-package vertico
  :demand
  ;; :general
  ;; (:keymaps 'vertico-map
  ;;           "C-j" #'vertico-next
  ;;           "C-k" #'vertico-previous)
  :config
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(savehist-mode)

;; A few more useful configurations...
(use-feature emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq display-line-numbers-type 'relative)
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; support Pinyin first character match for orderless, avy etc.
(use-package pinyinlib :ensure t)

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless partial-completion basic))
  (setq orderless-component-separator "[ &]") ; & is for company because space will break completion
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil)
  :config
  (defun completion--regex-pinyin (str) (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; minibuffer helpful annotations
(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package consult
  :ensure t
  :after org
  :bind (([remap goto-line]                     . consult-goto-line)
         ([remap isearch-forward]               . consult-line-symbol-at-point) ; my-consult-ripgrep-or-line
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap apropos]                       . consult-apropos)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap multi-occur]                   . consult-multi-occur)
         ([remap recentf-open-files]            . consult-recent-file)
         ("C-x j"                               . consult-mark)
         ("C-c g"                               . consult-ripgrep)
         ("C-c f"                               . consult-find)
         ("\e\ef"                               . consult-locate) ; need to enable locate first
         ("C-c n h"                             . my/consult-find-org-headings)
         :map org-mode-map
         ("C-c C-j"                             . consult-org-heading)
         :map minibuffer-local-map
         ("C-r"                                 . consult-history)
         :map isearch-mode-map
         ("C-;"                                 . consult-line)
         :map prog-mode-map
         ("C-c C-j"                             . consult-outline))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; MacOS locate doesn't support `--ignore-case --existing' args.
  (setq consult-locate-args (pcase system-type
                              ('gnu/linux "locate --ignore-case --existing --regex")
                              ('darwin "mdfind -name")))
  :config
  (consult-customize
    consult-theme
    :preview-key '(:debounce 0.2 any)
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
    :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  ;; Use `consult-ripgrep' instead of `consult-line' in large buffers
  (defun consult-line-symbol-at-point ()
    "Consult line the synbol where the point is"
    (interactive)
    (consult-line (thing-at-point 'symbol))))

(use-package embark
  :demand t
  :ensure t
  :general
  (:keymaps 'override
   "C-;" 'embark-dwim
   :states '(normal insert motion emacs)
   "C-." 'embark-act)

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package cape
  :ensure t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)  ; programming language keyword
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)   ; elisp symbol
  (add-to-list 'completion-at-point-functions #'cape-line)

  :config
  (setq cape-dict-file (expand-file-name "etc/hunspell_dict.txt" user-emacs-directory))

  ;; for Eshell:
  ;; ===========
  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; and behaves as a pure `completion-at-point-function'.
  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(provide 'init-complete)

;;; init-complete.el ends here
