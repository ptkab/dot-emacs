;;; appearance.el --- Changes Look and Feel of Emacs -*- lexical-binding: t -*-


;;; Commentary:
;; Improves the look and feel of Emacs.


;;; Code:

;; Set a dedicate custom file for Emacs.
(defvar user-custom-file (expand-file-name "custom-file.el" user-emacs-directory))
(unless (file-exists-p user-custom-file)
  (with-temp-buffer (write-file user-custom-file)))
(setq custom-file user-custom-file)

;; Set the title of frame as buffer(file) name and its mode.
(setq-default frame-title-format '("%b"))

;; Set font face and size.
(set-face-attribute 'default nil
					:height 150
					:family "Geist Mono"
					:weight 'normal)

(use-package visual-fill-column
  :config
  (global-visual-fill-column-mode 1))

(use-package adaptive-wrap
  :hook
  (markdown-mode . adaptive-wrap-prefix-mode)
  (org-mode . adaptive-wrap-prefix-mode))

(use-package emacs
  :ensure nil
  :hook
  (emacs-startup . (lambda () (global-visual-line-mode 1))))

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

(use-package doom-themes
  :custom
  (doom-themes-enable-italic nil)
  :config
  (doom-themes-org-config))

;; Automatically toggle between light and dark themes based on time of the day.
(setq calendar-latitude 37.338207)
(setq calendar-longitude -121.886330)
(use-package circadian
  :after (doom-themes)
  :config
  (setq circadian-themes '((:sunrise . doom-tomorrow-day)
                           (:sunset  . doom-nova)))
  (circadian-setup))

(use-package doom-modeline
  :custom
  (doom-modeline-enable-word-count t)
  (doom-modeline-indent-info t)
  (doom-modeline-height 20)
  (doom-modeline-buffer-file-name-style 'filename)
  ;; visual-fill-colum affects modeline too. This pushes the right edge of mode-line to right-fringe.
  (mode-line-right-align-edge 'right-fringe)
  :config (doom-modeline-mode t))

(use-package solaire-mode
  :after (doom-themes)
  :config
  ;; (add-to-list 'solaire-mode-themes-to-face-swap "^doom-")
  (add-to-list 'solaire-mode-themes-to-face-swap 'doom-nova)
  (solaire-global-mode 1))

(use-package spacious-padding
  :init
  (setq spacious-padding-widths
        '( :internal-border-width 10
           :tab-width 4
           :right-divider-width 5
           :fringe-width 8))
  :config
  (spacious-padding-mode 1))

(provide 'appearance)
;;; appearance.el ends here.
