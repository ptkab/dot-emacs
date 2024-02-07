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

(provide 'doc)
;; doc.el ends here.
