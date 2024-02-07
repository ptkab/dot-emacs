;;; core.el --- Adds Completion and Core Tools -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;; Prompts the next possible keystroke after a prefix key.
(use-package which-key
  :custom
  (which-key-add-column-padding 1)
  (which-key-sort-uppercase-first nil)
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))

(use-package flycheck
  :disabled
  :custom
  (flycheck-display-errors-delay 0.2)
  :config (global-flycheck-mode))

(use-package undo-tree
  :config
  (defvar custom-undo-tree-directory (expand-file-name "undo/" user-emacs-directory))
  (setq undo-tree-history-directory-alist `(("." . ,custom-undo-tree-directory)))
  (global-undo-tree-mode t))

(use-package move-text
  :config
  ;; Indent after moving
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice)
  :bind
  ("s-<up>"   . move-text-up)
  ("s-<down>" . move-text-down))

(use-package expand-region
  :bind
  ("M-s-<down>" . er/expand-region)
  ("M-s-<up>" . er/contract-region))

(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-:" . avy-goto-line)))

;; Setup completion in Emacs with Vertico+Consult+Orderless+Embark+Marginalia+Corfu

;; Vertico uses savehist to sort the completion options.
(use-package savehist
  :elpaca nil
  :config
  (savehist-mode))

;; Basically Ivy.
(use-package vertico
  :init
  (setq enable-recursive-minibuffers t)
  :custom
  (vertico-cycle t)
  :config
  (vertico-indexed-mode)
  (vertico-mode)
  (setq vertico-multiform-commands
	'((consult-line
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
           (vertico-posframe-border-width . 10)
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (t posframe)))
  (vertico-multiform-mode 1))

(use-package vertico-posframe
  :init
  (setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
  :config
  (vertico-posframe-mode))

;; Basically Ivy-rich.
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away.
  :init
  (marginalia-mode))

;; Alternative to Company. Uses Cape for completion backends.
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.2)         ;; Delay after which corfu menu will show up
  (corfu-auto-prefix 2)          ;; Minimum number of chars for corfu to show menu
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-min-width 45)           ;; Minimum width of the completion buffer
  :config
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  (corfu-echo-mode)
  (corfu-indexed-mode)
  (global-corfu-mode)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'core)
;;; core.el ends here.
