;;; init.el --- Loads Emacs config -*- lexical-binding: t -*-


;;; Code:

;; Add the `lisp` directory to load-path where I have split my config into multiple files.
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Setup hotkeys for finding and (re)loading the Emacs config quickly.
(bind-key "C-c z 1" (lambda ()
		      (interactive)
		      (message "Emacs config")
		      (find-file user-init-file)))

(bind-key "C-c z 2" (lambda ()
		      (interactive)
		      (load-file user-init-file)))

;; Load each config file individually.
(require 'bootstrap)
(require 'appearance)
(require 'core)

;; Vertico+Consult+Orderless+Embark+Marginalia+Corfu

(provide 'init)
;;; init.el ends here
