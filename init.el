;;; init.el --- Loads Emacs config -*- lexical-binding: t -*-


;;; Code:

;; Add the `lisp` directory to load-path where I have split my config into multiple files.
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Load each config file individually.
(require 'bootstrap)
(require 'appearance)
(require 'core)
(require 'dev)
(require 'org-config)

;; Setup hotkeys for finding and (re)loading the Emacs config quickly.
(bind-key "C-c z 1" (lambda ()
		      "Open Emacs config."
		      (interactive)
		      (find-file user-init-file)))

(bind-key "C-c z 2" (lambda ()
		      "Reload Emacs config."
		      (interactive)
		      (load-file user-init-file)))

(provide 'init)
;;; init.el ends here
