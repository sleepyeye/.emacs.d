;;; builtin.el --- core builtins, tidy & fast -*- lexical-binding: t; -*-

;; --- Required built-ins -----------------------------------------------------
(require 'xref)
(require 'compile)

;; --- User info --------------------------------------------------------------
(setq user-full-name "sleepyeye"
      user-mail-address "wonjunlee.0729@gmail.com")

;; --- Backups / Autosave -----------------------------------------------------
(defconst sleepy/backup-kept-new-versions 6
  "Number of newest backup versions to keep.")

(defconst sleepy/backup-kept-old-versions 2
  "Number of oldest backup versions to keep.")

(setq make-backup-files t                ; Enable backups for safety
      backup-by-copying t                ; Don't clobber symlinks
      version-control t                  ; Use version numbers for backups
      delete-old-versions t              ; Don't ask to delete excess backups
      kept-new-versions sleepy/backup-kept-new-versions
      kept-old-versions sleepy/backup-kept-old-versions
      create-lockfiles nil               ; Disable lockfiles (optional: set to t for collision protection)
      vc-make-backup-files nil           ; Don't backup version-controlled files
      auto-save-default t
      auto-save-include-big-deletions t
      kill-buffer-delete-auto-save-files t)

;; Backup directory (separate from autosave)
(defconst sleepy/backup-dir (expand-file-name "backups/" user-emacs-directory))
(unless (file-directory-p sleepy/backup-dir)
  (make-directory sleepy/backup-dir t))

(setq backup-directory-alist `(("." . ,sleepy/backup-dir)))

;; Autosave directory
(defconst sleepy/autosave-dir (expand-file-name "autosave/" user-emacs-directory))
(defconst sleepy/tramp-autosave-dir (expand-file-name "tramp-autosave/" user-emacs-directory))
(unless (file-directory-p sleepy/autosave-dir) (make-directory sleepy/autosave-dir t))
(unless (file-directory-p sleepy/tramp-autosave-dir) (make-directory sleepy/tramp-autosave-dir t))

(setq auto-save-list-file-prefix sleepy/autosave-dir
      tramp-auto-save-directory sleepy/tramp-autosave-dir
      auto-save-file-name-transforms `((".*" ,sleepy/autosave-dir t)))

;; --- General UX -------------------------------------------------------------
(setq use-short-answers t
      confirm-kill-emacs 'yes-or-no-p
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      what-cursor-show-names t
      find-file-suppress-same-file-warnings t)

;; Load custom.el separately (after elpaca initialization)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

;; --- Appearance -------------------------------------------------------------
(defconst sleepy/line-numbers-width 3
  "Default width for line number display in columns.")

(defconst sleepy/tab-width 4
  "Number of spaces per tab character.")

(defconst sleepy/fill-column 80
  "Column beyond which automatic line-wrapping should happen.")

(setq-default display-line-numbers-width sleepy/line-numbers-width
              tab-width sleepy/tab-width
              truncate-lines t
              fill-column sleepy/fill-column)

(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(when (display-graphic-p)
  (setq-default frame-title-format nil
                use-dialog-box nil))

;; --- Encoding ---------------------------------------------------------------
(set-language-environment "English")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; --- Cursor / Blink / Visual noise -----------------------------------------
(defconst sleepy/delete-pair-blink-delay 0.03
  "Delay in seconds for blinking when deleting a pair of delimiters.")

(blink-cursor-mode -1)
(setq blink-matching-paren nil
      x-stretch-cursor nil
      delete-pair-blink-delay sleepy/delete-pair-blink-delay
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil)

;; --- Whitespace -------------------------------------------------------------
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; --- Electric quote mode (smart quotes for text/LaTeX) --------------------
(add-hook 'text-mode-hook #'electric-quote-mode)
(add-hook 'LaTeX-mode-hook #'electric-quote-mode)

;; --- External tools ---------------------------------------------------------
;; Prefer modern tools over traditional ones with proper fallback
(defconst sleepy/grep-program
  (cond ((executable-find "rg") "rg")
        ((executable-find "grep") "grep")
        (t nil))
  "Preferred grep program (ripgrep > grep).")

(defconst sleepy/find-program
  (cond ((executable-find "fd") "fd")
        ((executable-find "find") "find")
        (t nil))
  "Preferred find program (fd > find).")

(when sleepy/grep-program
  (setq grep-program sleepy/grep-program))

(when sleepy/find-program
  (setq find-program sleepy/find-program))

(unless sleepy/grep-program
  (display-warning 'builtin "No grep program found. Search functionality will be limited." :warning))

(unless sleepy/find-program
  (display-warning 'builtin "No find program found. File search functionality will be limited." :warning))

;; --- Compilation ------------------------------------------------------------
(setq compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output t)

;; --- Auto-revert ------------------------------------------------------------
(setq revert-without-query (list ".")
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t
      global-auto-revert-non-file-buffers t
      auto-revert-check-vc-info t)

;; --- recentf / savehist / saveplace ----------------------------------------
(defconst sleepy/recentf-max-items 100
  "Maximum number of recent files to remember.")

(defconst sleepy/history-length 300
  "Maximum length of minibuffer history.")

(defconst sleepy/save-place-limit 600
  "Maximum number of saved places to remember.")

(setq recentf-max-saved-items sleepy/recentf-max-items
      recentf-auto-cleanup 'never    ; Manual cleanup only (more reliable)
      recentf-show-file-shortcuts-flag t
      history-length sleepy/history-length
      savehist-save-minibuffer-history t
      save-place-file (expand-file-name "saveplace" user-emacs-directory)
      save-place-limit sleepy/save-place-limit)

;; --- Better Defaults --------------------------------------------------------
;; Uniquify: better buffer names when files have same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; ibuffer: improved buffer list
(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil
      ibuffer-use-other-window nil)

;; Better scrolling
(defconst sleepy/scroll-conservatively 101
  "Scroll threshold to avoid recentering.
Values > 100 prevent automatic recentering when cursor moves off screen.")

(setq scroll-conservatively sleepy/scroll-conservatively
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; --- Ediff ------------------------------------------------------------------
(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

;; --- macOS ls-lisp/date formatting -----------------------------------------
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil
        ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d      ")))

(setq calendar-date-style 'iso)

;; --- Mode hooks (consolidated in after-init) --------------------------------
(add-hook 'after-init-hook #'global-auto-revert-mode)
(add-hook 'after-init-hook #'recentf-mode)
(add-hook 'after-init-hook #'savehist-mode)
(add-hook 'after-init-hook #'save-place-mode)
(add-hook 'after-init-hook #'show-paren-mode)

;; show-paren configuration
(setq show-paren-delay 0.0
      show-paren-style 'parenthesis  ; or 'expression to highlight entire block
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;; imenu: Auto-rescan and sorting in certain modes
(dolist (hook '(markdown-mode-hook makefile-mode-hook prog-mode-hook))
  (add-hook hook
            (lambda ()
              (setq-local imenu-auto-rescan t)
              (when (derived-mode-p 'prog-mode)
                (setq-local imenu-sort-function #'imenu--sort-by-name)))))

;; --- macOS specific: GUI mode only ---
(when (and (eq system-type 'darwin) (display-graphic-p))
  (cond
   ((boundp 'ns-command-modifier)
    (setq ns-command-modifier 'super
          ns-option-modifier  'meta
          ns-function-modifier 'hyper))
   ((boundp 'mac-command-modifier)
    (setq mac-command-modifier 'super
          mac-option-modifier  'meta
          ns-function-modifier 'hyper)))
  (setq select-enable-clipboard t
        select-enable-primary  nil)
  (global-set-key (kbd "s-c") #'kill-ring-save)
  (global-set-key (kbd "s-v") #'yank)
  (global-set-key (kbd "s-x") #'kill-region))

;;; builtin.el ends here
