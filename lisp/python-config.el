;;; python-config.el --- Loads Python config -*- lexical-binding: t -*-

;; Code:
(use-package python
  :ensure nil
  :init
  (setq python-shell-interpreter "python3")
  (setq python-indent-offset 4)
  :hook
  (python-mode . lsp-deferred))

(use-package blacken
  :hook (python-mode . blacken-mode)
  :custom
  (blacken-line-length 80))

(use-package lsp-pyright
  :init
  (setq lsp-pyright-langserver-command "pyright")
  :custom
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-use-library-code-for-types t))

(use-package org
  :ensure nil
  :hook
  (org-src-mode . (lambda ()
                    (when (derived-mode-p 'python-mode)
                      (lsp)))))

(provide 'python-config)
;; python-config.el ends here.
