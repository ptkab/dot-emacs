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
  (treemacs-user-mode-line-format 'none)
  :config
  (treemacs-hide-gitignored-files-mode t)
  :hook
  (emacs-startup . treemacs)
  :bind
  ("C-M-0" . treemacs)
  ("M-0" . treemacs-select-window))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-responsive 'top)
  :hook
  (prog-mode . highlight-indent-guides-mode))

(use-package lsp-mode
  :commands (lsp lsp-defferred)
  :init
  (setq lsp-completion-provider :none)
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-indirect-buffer t)
  :config
  (lsp-enable-which-key-integration t)
  (lsp-enable-imenu))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable t))

(use-package magit
  :bind
  ("C-x g" . magit-status))


(provide 'dev)
;; dev.el ends here.
