;;; doc.el --- Document config for Emacs -*- lexical-binding: t -*-

;; Commentary:
;; Config for handling documents like PDFs or Markdown etc.

;; Code:

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  (setq-default pdf-view-display-size 'fit-page))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :config
  (setq markdown-command "pandoc"))

(use-package writeroom-mode
  :disabled
  :custom
  (writeroom-major-modes '(text-mode markdown-mode org-mode))
  (writeroom-global-effects '(writeroom-set-alpha
                              writeroom-set-menu-bar-lines
                              writeroom-set-tool-bar-lines
                              writeroom-set-vertical-scroll-bars))
  (writeroom-width 0.9)
  :hook
  (org-mode . writeroom-mode)
  (markdown-mode . writeroom-mode)
  (text-mode . writeroom-mode))

(use-package olivetti
  :disabled
  :custom
  (olivetti-style 'fancy)
  (olivetti-body-width 0.75)
  :hook
  (org-mode . olivetti-mode))

(provide 'doc)
;; doc.el ends here.
