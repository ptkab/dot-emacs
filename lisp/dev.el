;;; dev.el --- Development config for Emacs -*- lexical-binding: t -*-


;;; Code:

;; Automatically format the code.
(use-package format-all
  :hook
  (prog-mode . format-all-mode)
  (prog-mode . format-all-ensure-formatter))

(provide 'dev)
;; dev.el ends here.
