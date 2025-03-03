;;; early-init.el --- Early init load config for Emacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Turn the UI elements off before UI initialization instead of after.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Disable the built in package.el package manager. I use Straight instead.
(setq package-enable-at-startup nil)
(setq inhibit-default-init nil)

;; Increase the garbage collector threshold during startup for faster Emacs loading.
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold))

;; Disable annoying native compilation warning messages.
(setq native-comp-async-report-warnings-errors nil)

;; When both .el and .elc/.eln files are available, load the latest one.
(setq load-prefer-newer t)

;; Start Emacs in maximized frame.
(push '(fullscreen . maximized) default-frame-alist)

(setq frame-resize-pixelwise t)

;; Don't bug me with ring bell
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)

(provide 'early-init)
;;; early-init.el ends here.
