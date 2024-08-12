;;; org-config.el --- Loads Org mode config -*- lexical-binding: t -*-

;; Code:

(use-package org
  :init
  (setq org-directory "~/org")

  (setq-default
   org-agenda-block-separator ?-             ;; Add a line to separate agenda items in dinstint sections
   org-agenda-max-entries 10                 ;; Show at max 10 entries from my agenda
   org-agenda-skip-scheduled-if-done t       ;; No need to honor schedule if the task is already done
   org-auto-align-tags t                     ;; Align the tags automatically
   org-cycle-separator-lines 2               ;; Leave a line between org items
   org-enforce-todo-checkbox-dependencies t  ;; Enforce checklist items should be completed in order
   org-enforce-todo-dependencies t           ;; Enforce tasks should be completed in order
   org-hide-emphasis-markers t               ;; Hide the Org markup indicators
   org-hide-leading-stars t                  ;; Don't hide leading stars. Org-supertsar will take care of this
   org-insert-heading-respect-content t      ;; Create Org header in the current subtree
   org-log-into-drawer t                     ;; Use LOGBOOK drawer to take notes
   org-pretty-entities t                     ;; Show UTF entities
   org-special-ctrl-a/e t                    ;; C-{a,e} should behave differently on headings
   org-src-fontify-natively t                ;; Enable source code highlighting
   org-src-preserve-indentation t            ;; Emacs weirdly indents code blocks otherwise
   org-startup-indented t                    ;; Auto indent the body under headlines
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))

  ;; I use Amazon SIM style statuses with some changes.
  (setq org-todo-keywords
	'((sequence "TODO(t)"
                    "IMPLEMENTATION(i)"
                    "BLOCKED(b)"
                    "REVIEW(r)"
                    "|"
                    "DELEGATED(l)"
                    "DONE(d)")))

  (setq org-babel-python-command "python3")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))

  (defun pratik/org-adjust-tags-column ()
    "Dynamically adjust `org-tags-column` to right-align tags."
    (setq org-tags-column (- (- (window-width) 4))))

  :hook
  ;; Auto format org files before saving.
  (org-mode . (lambda ()
		(add-hook 'before-save-hook #'org-fill-paragraph nil t)))
  (org-mode . pratik/org-adjust-tags-column)
  (window-configuration-change . pratik/org-adjust-tags-column)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)))

;; (add-hook 'org-mode-hook 'turn-on-flyspell)
;; (add-hook 'org-mode-hook #'org-indent-mode)

(use-package org-superstar
  :after org
  :custom
  (org-superstar-special-todo-items t)
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package org-roam
  :custom
  (setq org-roam-directory "~/org/notes")
  :bind (("C-c n l")))

(provide 'org-config)
;; org-config.el ends here.
