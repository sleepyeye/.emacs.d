;; backup, lock and custom files
(setq
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 delete-old-versions t
 backup-by-copying t
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(setq create-lockfiles nil
      auto-save-default t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


(recentf-mode 1)
(global-hl-line-mode +1)



(if IS-MAC
    (setq mac-option-modifier 'super
	  mac-command-modifier 'meta))

;; Make utf-8 default from @rougier
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment

(setq-default tab-width 2)
(setq make-backup-files nil)
