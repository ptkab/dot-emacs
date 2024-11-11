;;; bootstrap.el --- Bootstrap Emacs config -*- lexical-binding: t -*-


;;; Commentary:
;; Bootstraps Emacs startup with better defaults and a package manager.


;;; Code:

;; Bootstrap code for downloading and setting up 'straight.el' package manager.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/master/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load org mode as soon as `straight.el' is loaded.
(straight-use-package 'org)

;; Make 'use-package' macro use 'straight.el' under the hood.
(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t))
(setq use-package-always-ensure t)

;; Sensible defaults is a collection of better default functions and keybindings.
(require 'sensible-defaults)
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)

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
(setq-default visual-line-mode t
			  fill-column 80
			  auto-fill-mode t)
(setq font-lock-maximum-decoration t)
(setq scroll-preserve-screen-position t)
(global-hl-line-mode t)
(blink-cursor-mode -1)

(setq-default left-fringe-width 0
			  right-fringe-width 0)

;; I don't want autosave and backup files that Emacs creates.
(setq-default
 auto-save-default nil
 auto-save-list-file-prefix nil
 make-backup-files nil
 create-lockfiles nil)

;; Open buffers from previous session by default when I start Emacs.
(setq-default desktop-dirname user-emacs-directory)
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
(setq-default tab-width 4)

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
(bind-key "M-o" 'other-window)

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
