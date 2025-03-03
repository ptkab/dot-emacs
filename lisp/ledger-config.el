;;; ledger-config.el --- Ledger Mode Config -*- lexical-binding: t -*-


;;; Code:
(use-package ledger-mode)

(use-package flycheck-ledger
  :after (ledger-mode))

(provide 'ledger-config)
