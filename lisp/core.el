;;; core.el --- Adds Completion and Core Tools -*- lexical-binding: t -*-

;;; Commentary:


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

(use-package flycheck
  :disabled
  :custom
  (flycheck-display-errors-delay 0.2)
  :config (global-flycheck-mode))

(use-package undo-tree
  :config
  (defvar custom-undo-tree-directory (expand-file-name "undo/" user-emacs-directory))
  (setq undo-tree-history-directory-alist `(("." . ,custom-undo-tree-directory)))
  (global-undo-tree-mode t))

(use-package move-text
  :config
  ;; Indent after moving
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice)
  :bind
  ("s-<up>"   . move-text-up)
  ("s-<down>" . move-text-down))

(use-package expand-region
  :bind
  ("M-s-<down>" . er/expand-region)
  ("M-s-<up>" . er/contract-region))

(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-:" . avy-goto-line)))

(use-package recentf
  :bind
  ("C-x C-r" . recentf)
  :config
  (recentf-mode t))

;; Setup completion in Emacs with Vertico+Consult+Orderless+Embark+Marginalia+Corfu

;; Vertico uses savehist to sort the completion options.
(use-package savehist
  :config
  (savehist-mode))

;; Basically Ivy.
(use-package vertico
  :init
  (setq enable-recursive-minibuffers t)
  :custom
  (vertico-cycle t)
  :config
  (vertico-indexed-mode)
  (vertico-mode)
  (setq vertico-multiform-commands
	'((consult-line
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
           (vertico-posframe-border-width . 10)
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (t posframe)))
  (vertico-multiform-mode 1))

(use-package vertico-posframe
  :init
  (setq vertico-posframe-parameters
	'((left-fringe . 8)
          (right-fringe . 8)))
  :config
  (vertico-posframe-mode))

;; Basically Ivy-rich.
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away.
  :init
  (marginalia-mode))

;; Swiper probably?
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
	 ("C-c s" . consult-ripgrep)               ;; I am used to this key from Ivy-Counsel
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

;; Alternative to Company. Uses Cape for completion backends.
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.2)         ;; Delay after which corfu menu will show up
  (corfu-auto-prefix 2)          ;; Minimum number of chars for corfu to show menu
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-min-width 45)           ;; Minimum width of the completion buffer
  :config
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  (corfu-echo-mode)
  (corfu-indexed-mode)
  (global-corfu-mode)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))



(provide 'core)
;;; core.el ends here.
