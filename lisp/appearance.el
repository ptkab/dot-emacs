;;; appearance.el --- Changes Look and Feel of Emacs -*- lexical-binding: t -*-


;;; Commentary:
;; Improves the look and feel of Emacs.


;;; Code:

;; Set a dedicate custom file for Emacs.
(defvar user-custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(unless (file-exists-p user-custom-file)
  (with-temp-buffer (write-file user-custom-file)))
(setq custom-file user-custom-file)
(load-file custom-file)

;; Set the title of frame as buffer(file) name and its mode.
(setq-default frame-title-format '("%b [%m]"))

;; Set font face and size.
(set-face-attribute 'default nil :height 140)
(set-frame-font "JetBrains Mono")

;; Required for doom-modeline
(use-package nerd-icons
  :config
  (unless (member "Symbols Nerd Font" (font-family-list))
    (nerd-icons-install-fonts t)))

;; Show nice icons with Corfu completion.
(use-package nerd-icons-corfu
  :after corfu nerd-icons)

;; Show nice icons in the minibuffer with Marganalia.
(use-package nerd-icons-completion
  :after marginalia nerd-icons
  :config
  (nerd-icons-completion-mode)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; Nice icons in Treemacs.
(use-package treemacs-nerd-icons
  :after treemacs nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package doom-modeline
  :custom
  (doom-modeline-enable-word-count t)
  (doom-modeline-indent-info t)
  (doom-modeline-height 30)
  (doom-modeline-buffer-file-name-style 'filename)
  :config (doom-modeline-mode t))

;; Define helper function to toggle between light and dark mode themes and add a
;; keybinding to it.
(defvar pratik/light-theme)
(defvar pratik/dark-theme)
(defvar pratik/active-theme 'pratik/light-theme)
(defun pratik/toggle-dark-light-theme ()
  "Toggle between dark and light mode theme."
  (interactive)
  (disable-theme pratik/active-theme)
  (if (eq pratik/active-theme pratik/light-theme)
      (setq pratik/active-theme pratik/dark-theme)
    (setq pratik/active-theme pratik/light-theme))
  (load-theme pratik/active-theme t))
(global-set-key (kbd "C-x c") 'pratik/toggle-dark-light-theme)


(use-package doom-themes
  :disabled
  :config
  (doom-themes-org-config)
  (load-theme 'doom-vibrant t))

;; Dim the auxiliary buffers like Treemacs.
(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package spacemacs-theme
  :config
  (require 'spacemacs-theme)
  (deftheme spacemacs-light "Spacemacs light theme")
  (deftheme spacemacs-dark "Spacemacs dark theme")
  (create-spacemacs-theme 'light 'spacemacs-light)
  (create-spacemacs-theme 'dark 'spacemacs-dark)
  (provide-theme 'spacemacs-light)
  (provide-theme 'spacemacs-dark)
  (setq pratik/light-theme 'spacemacs-light
	pratik/dark-theme 'spacemacs-dark)
  (load-theme 'spacemacs-dark t))

(provide 'appearance)
;;; appearance.el ends here.
