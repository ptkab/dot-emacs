;;; pratik.el --- My personal Emacs config and settings -*- lexical-binding: t -*-


;;; Code:

;; Setup hotkeys for finding and (re)loading the Emacs config quickly.
(bind-key "C-c z 1" (lambda ()
                      "Open Emacs config."
                      (interactive)
                      (find-file user-init-file)))

(bind-key "C-c z 2" (lambda ()
                      "Reload Emacs config."
                      (interactive)
                      (load-file user-init-file)))

(provide 'pratik)
