;;; appearance.el --- Changes Look and Feel of Emacs -*- lexical-binding: t -*-


;;; Commentary:
;; Improves the look and feel of Emacs.


;;; Code:

;; Set a dedicate custom file for Emacs.
(defvar user-custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(unless (file-exists-p user-custom-file)
  (with-temp-buffer (write-file user-custom-file)))
(setq custom-file user-custom-file)

;; Set a toggle key to switch between light and dark variant of current theme.
(defvar pratik/light-theme)
(defvar pratik/dark-theme)
(defun pratik/toggle-dark-light-theme ()
  "Toggle between dark and light mode theme."
  (interactive)
  (defvar pratik/active-theme (car custom-enabled-themes))
  (disable-theme pratik/active-theme)
  (if (eq pratik/active-theme pratik/light-theme)
      (setq pratik/active-theme pratik/dark-theme)
    (setq pratik/active-theme pratik/light-theme))
  (load-theme pratik/active-theme t))
;; I am currently not using this.
;; (bind-key "C-x c" 'pratik/toggle-dark-light-theme)

;; Set the title of frame as buffer(file) name and its mode.
(setq-default frame-title-format '("%b [%m]"))

;; Default foreground color (grey50) of column-indicator stands out too much.
(set-face-attribute 'fill-column-indicator nil :foreground "grey90")


;; Set font face and size.
(set-face-attribute 'default nil :height 150)
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

;; Dim the auxiliary buffers like Treemacs.
(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package spacemacs-theme
  :config
  (load-theme 'spacemacs-light t))

(provide 'appearance)
;;; appearance.el ends here.
