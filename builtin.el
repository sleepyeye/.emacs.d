;;; builtin.el --- builtin.el -*- no-byte-compile: t; lexical-binding: t; -*-

;; User Information
(setq user-full-name "sleepyeye"
      user-mail-address "wonjunlee.0729@gmail.com")

;; Backup and Lock Files
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq create-lockfiles nil
      make-backup-files nil
      backup-by-copying t
	  backup-by-copying-when-linked t
      auto-save-default t
      delete-old-versions t
      kept-new-versions 4
      kept-old-versions 2
      version-control t
	  vc-make-backup-files nil)


;; Auto save stuffs
;; Do not auto-disable auto-save after deleting large chunks of
;; text. The purpose of auto-save is to provide a failsafe, and
;; disabling it contradicts this objective.
(setq auto-save-include-big-deletions t)
(setq kill-buffer-delete-auto-save-files t)
(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))



;; Customization Settings
(setq use-short-answers t
      confirm-kill-emacs 'yes-or-no-p
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      what-cursor-show-names t)

;; Load Custom File
(setq custom-file null-device)

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
(setq compilation-always-kill t
      compilation-ask-about-save nil
	  compilation-scroll-output t
      compilation-scroll-output 'first-error)


;; Disable the warning "X and Y are the same file". Ignoring this warning is
;; acceptable since it will redirect you to the existing buffer regardless.
(setq find-file-suppress-same-file-warnings t)


;;; Auto-revert
;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

;; Revert other buffers (e.g, Dired)
(setq global-auto-revert-non-file-buffers t)



;;; recentf
;; `recentf' is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-auto-cleanup 'mode)

;;; saveplace
;; `save-place-mode` enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)

;;; savehist
;; `savehist` is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(setq history-length 300)
(setq savehist-save-minibuffer-history t)  ;; Default

;;; Cursor
;; The blinking cursor is distracting and interferes with cursor settings in
;; some minor modes that try to change it buffer-locally (e.g., Treemacs).
;; Additionally, it can cause freezing, especially on macOS, for users with
;; customized and colored cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; This controls how long Emacs will blink to show the deleted pairs with
;; `delete-pair'. A longer delay can be annoying as it causes a noticeable pause
;; after each deletion, disrupting the flow of editing.
(setq delete-pair-blink-delay 0.03)

;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines, which is useful for writing
;; longer comments or docstrings that span multiple lines.
(setq comment-multi-line t)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance,
;; ensuring that comments apply uniformly to all lines, including those that are
;; otherwise empty.
(setq comment-empty-lines t)

;;; Dired configs
(setq dired-clean-confirm-killing-deleted-buffers nil
      dired-kill-when-opening-new-dired-buffer t
      dired-recursive-deletes 'top
      dired-recursive-copies  'always
      dired-create-destination-dirs 'ask)


;;; Ediff

;; Configure Ediff to use a single frame and split windows horizontally
(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)




;; Required Packages
(require 'xref)


;;; Date and formatting
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
