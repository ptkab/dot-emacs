;;; init.el --- Loads Emacs config -*- lexical-binding: t -*-


;;; Code:

;; Add the `lisp` directory to load-path where I have split my config into multiple files.
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Load each config file individually.
(require 'bootstrap)
(require 'appearance)
(require 'core)
(require 'dev)
(require 'doc)
(require 'org-config)
(require 'ledger-config)
(require 'python-config)
(require 'pratik)

;;; init.el ends here
