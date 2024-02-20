;;; bootstrap.el --- Bootstrap Emacs config -*- lexical-binding: t -*-


;;; Commentary:
;; Bootstraps Emacs startup with better defaults and a package manager.


;;; Code:

;; Install the Elpaca package manager.
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Add MELPA submissions to elpaca queue
(elpaca-queue
 (elpaca (melpulls :host github :repo "progfolio/melpulls" :protocol ssh)
   (add-to-list 'elpaca-menu-functions 'melpulls)))

;; Use the `use-package` macro with Elpaca
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))
(elpaca-wait)

;; (use-package org
;;   :ensure t
;;   :config (org-mode))
;; (elpaca-wait)

;; Sensible defaults is a collection of better default functions and keybindings.
(elpaca (sensible-defaults
         :host github
         :repo "hrs/sensible-defaults.el"))
(add-hook 'elpaca-after-init-hook
	  (lambda ()
	    (load-file
             (expand-file-name "elpaca/builds/sensible-defaults/sensible-defaults.el" user-emacs-directory))
            (sensible-defaults/use-all-settings)
            (sensible-defaults/use-all-keybindings)))

;; Set buffer margins
(setq-default left-margin-width 1 right-margin-width 1)
(set-window-buffer nil (current-buffer))

(setq x-underline-at-descent-line t)

;; Set the UTF-8 as default encoding
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Make Emacs a better code editor as well as a text editor.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'column-number-mode)
(electric-pair-mode 1)
(turn-on-visual-line-mode)
(turn-on-auto-fill)
(setq font-lock-maximum-decoration t)
(setq scroll-preserve-screen-position t)
(global-hl-line-mode t)
(blink-cursor-mode -1)

;; I don't want autosave and backup files that Emacs creates.
(setq-default
 auto-save-default nil
 auto-save-list-file-prefix nil
 make-backup-files nil
 create-lockfiles nil)

;; Open buffers from previous session by default when I start Emacs.
(desktop-save-mode 1)

;; Kill and yank settings. I override how kill ring functions in
(setq-default
 select-enable-clipboard t
 mouse-yank-at-point t
 kill-whole-line t)

;; Automatically switch focus to Help buffer whenever one opens, so that I can
;; easily close it by pressing 'q' when I am done taking help.
(setq-default help-window-select t)

;; Default completion settings.
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)
;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;; Corfu commands are hidden, since they are not supposed to be used via M-x.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)
;; Enable indentation+completion using the TAB key.
(setq tab-always-indent 'complete)

;; Ignore buffers that start with '*' when switching buffers.
(set-frame-parameter (selected-frame) 'buffer-predicate
                     (lambda (buf) (not (string-match-p "^*" (buffer-name buf)))))

;; Use Option key as Super on Mac.
(setq mac-option-modifier 'super)

;; Better keybindings for buffer management.
(bind-key "M-`" 'other-frame)
(bind-key "RET" 'newline-and-indent)
(bind-key "C-x k" 'kill-current-buffer)

;; Better bindings for movement.
(bind-key "s-<left>" 'left-word)
(bind-key "s-<right>" 'right-word)
(bind-key "M-<left>" 'move-beginning-of-line)
(bind-key "M-<right>" 'move-end-of-line)
(bind-key "M-<up>" 'beginning-of-buffer)
(bind-key "M-<down>" 'end-of-buffer)
(bind-key "M-[" 'previous-buffer)
(bind-key "M-]" 'next-buffer)

;; Better bindings for editing.
(bind-key "M-/" 'sensible-defaults/comment-or-uncomment-region-or-line)
(bind-key "M-z" 'undo)
(bind-key "M-Z" 'undo-redo)
(bind-key "M-d" 'mark-word)

;; Sync the sytem PATH variable with Emacs.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))


(provide 'bootstrap)
;;; bootstrap.el ends here.
