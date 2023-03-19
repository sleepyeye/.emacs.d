; backup, lock and custom files
(setq create-lockfiles nil
	  make-backup-files nil
      backup-by-copying t
      auto-save-default t
      delete-old-versions t
      kept-new-versions 4
      kept-old-versions 2
      version-control t)

(setq use-short-answers t) 
(setq confirm-kill-emacs 'yes-or-no-p) 
(setq initial-scratch-message ""
      initial-buffer-choice t)

(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)


(setq-default display-line-numbers-width 3)
(setq-default tab-width 4)

(setq dired-kill-when-opening-new-dired-buffer t)
(setq what-cursor-show-names t) ;; improves C-x =



(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


(recentf-mode 1)
(global-hl-line-mode +1)

;; Make utf-8 default from @rougier
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment




(use-package gcmh
  :demand t
  :diminish gcmh-mode
  :init
  (gcmh-mode 1)
  :config
  (setq gcmh-idle-delay 5))
(elpaca-wait)
