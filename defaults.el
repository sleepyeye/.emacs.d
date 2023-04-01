;; Backup, lock, and custom files
(setq create-lockfiles nil
	  make-backup-files nil
	  backup-by-copying t
	  auto-save-default t
	  delete-old-versions t
	  kept-new-versions 4
	  kept-old-versions 2
	  version-control t)

;; Customization
(setq use-short-answers t
	  confirm-kill-emacs 'yes-or-no-p
	  initial-scratch-message ""
	  initial-buffer-choice t
	  save-interprogram-paste-before-kill t
	  apropos-do-all t
	  mouse-yank-at-point t
	  dired-kill-when-opening-new-dired-buffer t
	  what-cursor-show-names t
	  custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;; Appearance
(setq-default display-line-numbers-width 3
			  tab-width 4)
(recentf-mode 1)
(global-hl-line-mode +1)

;; Encoding
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "English")

;; Packages
(use-package gcmh
  :demand t
  :diminish gcmh-mode
  :init
  (gcmh-mode 1)
  :config
  (setq gcmh-idle-delay 5))

(elpaca-wait)
