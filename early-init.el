;;; early-init.el --- Emacs pre-initialization config -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; 设置垃圾回收参数
(setq gc-cons-threshold most-positive-fixnum
    gc-cons-percentage 0.6)

;; 启动早期不加载`package.el'包管理器
(setq package-enable-at-startup nil
    package-quickstart nil)

;; 禁止展示菜单栏、工具栏和纵向滚动条
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;; 禁止菜单栏、工具栏、滚动条模式，禁止启动屏幕和文件对话框
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)
(setq comp-deferred-compilation nil)

(defconst windows? (memq system-type '(cygwin windows-nt ms-dos)) "are we on windows")
(defconst macos? (eq system-type 'darwin) "are we on macos")
(defconst linux? (memq system-type '(gnu gnu/linux gnu/kfreebds berkeley-unix)) "are we on linux")
(defconst bsd? (memq system-type '(darwin berkeley-unix gnu/kfreebsd)) "are we on bsd")
(defconst wsl? (and linux? (string-match-p "microsoft" operating-system-release)) "are we on wsl")

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
