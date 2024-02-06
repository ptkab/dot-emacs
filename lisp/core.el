;;; core.el --- Adds Completion and Core Tools -*- lexical-binding: t -*-


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



;; Vertico+Consult+Orderless+Embark+Marginalia+Corfu

(provide 'core)
;;; core.el ends here
