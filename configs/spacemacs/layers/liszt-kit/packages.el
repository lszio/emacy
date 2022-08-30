;;; packages.el --- liszt-kit layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Liszt21 <1832666492@qq.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `liszt-kit-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `liszt-kit/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `liszt-kit/pre-init-PACKAGE' and/or
;;   `liszt-kit/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst liszt-kit-packages
  '(
    rime
    leetcode
    (popweb :location (recipe
                       :fetcher github
                       :repo "manateelazycat/popweb"
                       :files ("*"))))
  "The list of Lisp packages required by the liszt-kit layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun liszt-kit/init-rime ()
  (use-package rime
    :defer t
    :custom
    (rime-user-data-dir (if windows?
                            "C:/Users/Liszt/AppData/Roaming/Rime"
                            (if wsl? "/mnt/c/Users/Liszt/AppData/Roaming/Rime" "~/.config/fcitx/rime")))
    (default-input-method "rime")
    (rime-show-candidate 'posframe)
    (rime-disable-predicates '(rime-predicate-after-alphabet-char-p) ;; 在英文字符串之后（必须为以字母开头的英文字符串）
                              ;; rime-predicate-after-ascii-char-p ;; 任意英文字符后
                              rime-predicate-prog-in-code-p ;; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
                              rime-predicate-in-code-string-p ;; 在代码的字符串中，不含注释的字符串。
                              rime-predicate-evil-mode-p ;; 在 evil-mode 的非编辑状态下
                              ;; rime-predicate-ace-window-p ;; 激活 ace-window-mode
                              ;; rime-predicate-hydra-p ;; 如果激活了一个 hydra keymap
                              ;; rime-predicate-current-input-punctuation-p ;; 当要输入的是符号时
                              ;; rime-predicate-punctuation-after-space-cc-p ;; 当要在中文字符且有空格之后输入符号时
                              ;; rime-predicate-punctuation-after-ascii-p ;; 当要在任意英文字符之后输入符号时
                              ;; rime-predicate-punctuation-line-begin-p ;; 在行首要输入符号时
                              rime-predicate-space-after-ascii-p ;; 在任意英文字符且有空格之后
                              rime-predicate-space-after-cc-p ;; 在中文字符且有空格之后
                              rime-predicate-current-uppercase-letter-p ;; 将要输入的为大写字母时
                              rime-predicate-tex-math-or-command-p);; 在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
    :bind
    (:map rime-mode-map
      ("C-," . 'rime-force-enable))))

(defun liszt-kit/init-leetcode ()
  (use-package leetcode
    :custom
    (leetcode-prefer-language "python")
    (leetcode-prefer-sql "sqlite")
    (leetcode-save-solution t)
    (leetcode-directory "~/Notes/Program/LeetCode")))

(defun liszt-kit/init-popweb ()
  (use-package popweb)
  (with-eval-after-load 'popweb
    (message "Popweb loaded, loading extensions")

    (defun get-popweb-extension-paths (&rest extensions)
      (catch 'done
        (dolist (path load-path)
          (when (string-match "popweb" path)
            (throw 'done
                   (mapcar (lambda (p)
                             (concat path "/extension/" p))
                           extensions))))))

    ;; (defmacro load-popweb-extensions ()
    ;;   ;; FIXME 不能加載math-in-point.el
    ;;   (let ((extension-paths (get-popweb-extension-paths "dict" "latex")))
    ;;     `(progn
    ;;            (use-package popweb-dict-youdao :load-path ,(car extension-paths))
    ;;            (use-package popweb-latex :load-path ,(cadr extension-paths)))))

    (let ((extension-paths (get-popweb-extension-paths "dict" "latex")))
      (when (not (member (car extension-paths) load-path))
        (dolist (path extension-paths)
          (add-to-list 'load-path path))))

    (require 'popweb-dict-youdao)
    (require 'popweb-latex)))
