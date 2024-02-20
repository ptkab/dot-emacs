;;; dev.el --- Development config for Emacs -*- lexical-binding: t -*-


;;; Code:

;; Automatically format the code.
(use-package format-all
  :hook
  (prog-mode . format-all-mode)
  (prog-mode . format-all-ensure-formatter))

;; I use the default project.el not projectile.
(use-package project
  :ensure nil
  :bind ("C-x f" . project-find-file))

(use-package treemacs
  :custom
  (treemacs-width 45)
  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-project-follow-cleanup t)
  (treemacs-git-mode 'extended)
  (treemacs-filewatch-mode t)
  (treemacs-indent-guide-mode t)
  (treemacs-move-forward-on-expand t)
  :bind
  ("C-M-0" . treemacs)
  ("M-0" . treemacs-select-window))

(provide 'dev)
;; dev.el ends here.
