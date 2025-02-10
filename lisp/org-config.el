;;; org-config.el --- Loads Org mode config -*- lexical-binding: t -*-

;; Code:

(use-package org
  :ensure nil
  :init
  (setq org-directory "~/org")

  (setq-default
   ;; Add a line to separate agenda items in dinstint sections
   org-agenda-block-separator ?-
   ;; Show at max 10 entries from my agenda
   org-agenda-max-entries 10
   ;; No need to honor schedule if the task is already done
   org-agenda-skip-scheduled-if-done t
   ;; Align the tags automatically
   org-auto-align-tags t
   ;; Leave a line between org items
   org-cycle-separator-lines 2
   ;; Enforce checklist items should be completed in order
   org-enforce-todo-checkbox-dependencies t
   ;; Enforce tasks should be completed in order
   org-enforce-todo-dependencies t
   ;; Hide the Org markup indicators
   org-hide-emphasis-markers t
   ;; Don't hide leading stars. Org-supertsar will take care of this
   org-hide-leading-stars t
   ;; Create Org header in the current subtree
   org-insert-heading-respect-content t
   ;; Use LOGBOOK drawer to take notes
   org-log-into-drawer t
   ;; Show UTF entities
   org-pretty-entities t
   ;; C-{a,e} should behave differently on headings
   org-special-ctrl-a/e t
   ;; Enable source code highlighting
   org-src-fontify-natively t
   ;; Emacs weirdly indents code blocks otherwise
   org-src-preserve-indentation t
   ;; Auto indent the body under headlines
   org-startup-indented t
   ;; Default bullet style when demoting item
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+"))
   ;; Disable initial source code indentation
   org-edit-src-content-indentation 0
   ;; Org tags to align immediately after the heading
   org-tags-column 0)

  ;; I use Amazon SIM style statuses with some changes.
  (setq org-todo-keywords
		'((sequence "TODO(t)"
                    "IN-PROGRESS(i)"
					"WAITING(w)"
                    "BLOCKED(b)"
                    "REVIEW(r)"
                    "|"
                    "CANCELED(c)"
                    "DONE(d)")))

  (setq org-babel-python-command "python3")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (java . t)
     (shell . t)
     (js . t)))

  ;; (defun pratik/org-adjust-tags-column ()
  ;;   "Dynamically adjust `org-tags-column` to right-align tags."
  ;;   (setq org-tags-column (- (- (window-width) 4))))

  (defun pratik/set-org-mode-fringe ()
    (setq left-fringe-width 10
		  right-fringe-width 10)
    (set-face-attribute 'fringe nil
						:background (face-attribute 'default :background))
    (set-window-buffer nil (current-buffer)))

  :hook
  ;; Auto format org files before saving.
  ;; (org-mode . (lambda ()
  ;; (add-hook 'before-save-hook #'org-fill-paragraph)))
  ;; (org-mode . pratik/org-adjust-tags-column)
  ;; (window-configuration-change . pratik/org-adjust-tags-column)
  (org-mode . pratik/set-org-mode-fringe)
  (org-mode . flyspell-mode)

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

(use-package org-journal
  :init
  (setq org-journal-enable-entry-autodate nil)
  :custom
  (org-journal-dir (expand-file-name "journal/" org-directory))
  (org-journal-file-type 'daily)
  (org-journal-date-format "%A, %b %d %Y")
  (org-journal-file-format "%Y/%m/%m-%d-%Y.org")
  (org-journal-enable-agenda-integration t)
  (org-journal-time-format ""))

(provide 'org-config)
;; org-config.el ends here.
