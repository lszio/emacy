(use-package rime
  :defer t
  :custom
  ;; (rime-user-data-dir (if IS-LINUX (if IS-WSL "/mnt/c/Users/Liszt/AppData/Roaming/Rime" "~/.local/share/fcitx5/rime") "C:/Users/Liszt/AppData/Roaming/Rime"))
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates '(rime-predicate-after-alphabet-char-p ;; 在英文字符串之后（必须为以字母开头的英文字符串）
                             ;; rime-predicate-after-ascii-char-p ;; 任意英文字符后
                             rime-predicate-prog-in-code-p ;; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
                             rime-predicate-in-code-string-p ;; 在代码的字符串中，不含注释的字符串。
                             rime-predicate-evil-mode-p ;; 在 evil-mode 的非编辑状态下
                             ;; rime-predicate-ace-window-p ;; 激活 ace-window-mode
                             ;; rime-predicate-hydra-p ;; 如果激活了一个 hydra keymap
                             ;; rime-predicate-current-input-punctuation-p ;; 当要输入的是符号时
                             rime-predicate-punctuation-after-space-cc-p ;; 当要在中文字符且有空格之后输入符号时
                             rime-predicate-punctuation-after-ascii-p ;; 当要在任意英文字符之后输入符号时
                             rime-predicate-punctuation-line-begin-p ;; 在行首要输入符号时
                             rime-predicate-space-after-ascii-p ;; 在任意英文字符且有空格之后
                             rime-predicate-space-after-cc-p ;; 在中文字符且有空格之后
                             rime-predicate-current-uppercase-letter-p ;; 将要输入的为大写字母时
                             rime-predicate-tex-math-or-command-p));; 在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
  :bind
  (:map rime-mode-map
   ("C-," . 'rime-force-enable)))
