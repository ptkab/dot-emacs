;;; early-init.el --- Early init load config for Emacs -*- lexical-binding: t -*-

;;; Code:

;; Disable the built in package.el package manager. I use Elpaca instead.
(setq package-enable-at-startup nil)
(setq inhibit-default-init nil)

;; Increase the garbage collector threshold during startup for faster Emacs loading.
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold))

;; Disable annoying native compilation warning messages.
(setq native-comp-async-report-warnings-errors nil)

;; Turn the UI elements off before UI initialization instead of after.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Since I use Emacs from emacs-port with no title bar, I need this to drag
;; Emacs frame with mouse to another screen when using multiple displays.
(push '(drag-internal-border . 1) default-frame-alist)
(push '(internal-border-width . 1) default-frame-alist)

;; Start Emacs in maximized frame.
(push '(fullscreen . maximized) default-frame-alist)

;; Don't bug me with ring bell
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)

(setq desktop-restore-forces-onscreen nil)

(provide 'early-init)
;;; early-init.el ends here.
