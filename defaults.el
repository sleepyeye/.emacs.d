(setq user-full-name "sleepyeye"
      user-mail-address "wonjunlee.0729@gmaio.com")

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



(cond
 (IS-MAC
  ;; make title bar transparent and apply dark color
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))
 )


;; Enable imenu for use-package blocks
;; FIXME, seems not work alongside with elplaca
(setq use-package-enable-imenu-support t)

;; Stolen from @xenodium's config
(when (display-graphic-p)
  ;; No title. See init.el for initial value.
  (setq-default frame-title-format nil)
  ;; Hide the cursor in inactive windows.
  (setq cursor-in-non-selected-windows nil)
  ;; Avoid native dialogs.
  (setq use-dialog-box nil))


;; eat up all whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; prefer rg and fd over grep and find
(when (executable-find "rg")
  (setq grep-program "rg"))

(when (executable-find "fd")
  (setq find-program "fd"))

;; scroll compile output
(setq compilation-scroll-output t)

(require 'xref)
