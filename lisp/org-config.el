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
   ;; Enable language based completion with TAB in source blocks
   org-src-tab-acts-natively t
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

  :hook
  (org-mode . flyspell-mode)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)))

;; (add-hook 'org-mode-hook 'turn-on-flyspell)
;; (add-hook 'org-mode-hook #'org-indent-mode)

(use-package org-superstar
  :after org
  :init
  (setq org-superstar-headline-bullets-list '("●" "○" "▶" "▷"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package org-journal
  :init
  (setq org-journal-enable-entry-autodate nil)
  :custom
  (org-journal-find-file 'find-file)
  (org-journal-dir (expand-file-name "journal/" org-directory))
  (org-journal-file-type 'daily)
  (org-journal-date-format "%A, %b %d %Y")
  (org-journal-file-format "%Y/%m/%m-%d-%Y.org")
  (org-journal-enable-agenda-integration t)
  (org-journal-time-format "")
  :bind
  ("C-c j" . org-journal-new-entry))

(defun my/org-src-fix-lsp-filename ()
  "If the buffer is an indirect org-src buffer (e.g. for python)
that lacks a file name, assign it a dummy file name so that LSP
works."
  (when (and (derived-mode-p 'python-mode)
             (not buffer-file-name))
    ;; If the org file is saved, base the fake filename on its name.
    (let ((base (or (buffer-file-name (buffer-base-buffer))
                    (make-temp-file "org-src-" nil ".py"))))
      ;; Create a dummy file name that includes the buffer name.
      (setq-local buffer-file-name (concat base "::" (buffer-name)))
      ;; Also update the LSP buffer URI.
      (setq-local lsp--buffer-uri (lsp--path-to-uri buffer-file-name)))))

(add-hook 'org-src-mode-hook #'my/org-src-fix-lsp-filename)

(provide 'org-config)
;; org-config.el ends here.
