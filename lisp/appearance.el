;;; appearance.el --- Changes Look and Feel of Emacs -*- lexical-binding: t -*-


;;; Commentary:
;; Improves the look and feel of Emacs.


;;; Code:

;; Set a dedicate custom file for Emacs.
(defvar CUSTOM-FILE (expand-file-name "custom-file.el" user-emacs-directory))
(unless (file-exists-p CUSTOM-FILE)
  (with-temp-buffer (write-file CUSTOM-FILE)))
(setq custom-file CUSTOM-FILE)
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

(use-package doom-modeline
  :custom
  (doom-modeline-enable-word-count t)
  (doom-modeline-indent-info t)
  (doom-modeline-height 30)
  ;; (doom-modeline-def-modeline 'main
  ;;   '(bar matches buffer-info remote-host buffer-position parrot selection-info)
  ;;   '(misc-info minor-modes word-count battery lsp indent-info input-method buffer-encoding major-mode process vcs checker "  "))
  :config (doom-modeline-mode t))

(use-package doom-themes
  :config
  (load-theme 'doom-one-light t))

(use-package modus-themes
  :disabled
  :config
  (load-theme 'modus-operandi-tinted t))

;; Dim the auxiliary buffers like Treemacs.
(use-package solaire-mode
  :config
  (solaire-global-mode 1))


(provide 'appearance)
;;; appearance.el ends here.
