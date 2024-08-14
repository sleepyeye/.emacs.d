;; User Information
(setq user-full-name "sleepyeye"
      user-mail-address "wonjunlee.0729@gmail.com")

;; Backup and Lock Files
(setq create-lockfiles nil
      make-backup-files nil
      backup-by-copying t
      auto-save-default t
      delete-old-versions t
      kept-new-versions 4
      kept-old-versions 2
      version-control t)

;; Customization Settings
(setq use-short-answers t
      confirm-kill-emacs 'yes-or-no-p
      initial-scratch-message ""
      initial-buffer-choice t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      dired-kill-when-opening-new-dired-buffer t
      what-cursor-show-names t)

;; Load Custom File
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Appearance Settings
(setq-default display-line-numbers-width 3
              tab-width 4)
(recentf-mode 1)
(global-hl-line-mode +1)

;; Encoding Settings
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "English")

;; Graphics and UI
(when (display-graphic-p)
  (setq-default frame-title-format nil)
  (setq cursor-in-non-selected-windows nil)
  (setq use-dialog-box nil))

;; Whitespace and Formatting
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; External Tools
(when (executable-find "rg")
  (setq grep-program "rg"))
(when (executable-find "fd")
  (setq find-program "fd"))

;; Compilation Settings
(setq compilation-scroll-output t)

;; Required Packages
(require 'xref)

(use-package undo-fu)

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  ;; date format setting
  (setq ls-lisp-format-time-list
		'("%Y-%m-%d %H:%M"
		  "%Y-%m-%d      "))
  (setq ls-lisp-use-insert-directory-program nil))

;; Use year/month/day
(setq calendar-date-style 'iso)


(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)
