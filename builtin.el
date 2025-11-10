;;; builtin.el --- core builtins, tidy & fast -*- lexical-binding: t; -*-

;; --- Required built-ins -----------------------------------------------------
(require 'xref)
(require 'compile)

;; --- User info --------------------------------------------------------------
(setq user-full-name "sleepyeye"
      user-mail-address "wonjunlee.0729@gmail.com")

;; --- Backups / Autosave -----------------------------------------------------
(setq make-backup-files t                ; Enable backups for safety
      backup-by-copying t                ; Don't clobber symlinks
      version-control t                  ; Use version numbers for backups
      delete-old-versions t              ; Don't ask to delete excess backups
      kept-new-versions 6                ; Keep 6 newest versions
      kept-old-versions 2                ; Keep 2 oldest versions
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

;; custom.el 분리 로드 (elpaca 이후)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

;; --- Appearance -------------------------------------------------------------
(setq-default display-line-numbers-width 3
              tab-width 4
              truncate-lines t
              fill-column 80)

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
(blink-cursor-mode -1)
(setq blink-matching-paren nil
      x-stretch-cursor nil
      delete-pair-blink-delay 0.03
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil)

;; --- Whitespace -------------------------------------------------------------
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; --- Electric quote mode (smart quotes for text/LaTeX) --------------------
(add-hook 'text-mode-hook #'electric-quote-mode)
(add-hook 'LaTeX-mode-hook #'electric-quote-mode)

;; --- External tools ---------------------------------------------------------
(when (executable-find "rg") (setq grep-program "rg"))
(when (executable-find "fd") (setq find-program "fd"))

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
(setq recentf-max-saved-items 100
      recentf-auto-cleanup 'never    ; Manual cleanup only (more reliable)
      recentf-show-file-shortcuts-flag t
      history-length 300
      savehist-save-minibuffer-history t
      save-place-file (expand-file-name "saveplace" user-emacs-directory)
      save-place-limit 600)

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
(setq scroll-conservatively 101
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

;; --- Mode hooks (after-init에 일원화) ---------------------------------------
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

;; imenu: 일부 모드에서 자동 재스캔 + 정렬
(dolist (hook '(markdown-mode-hook makefile-mode-hook prog-mode-hook))
  (add-hook hook
            (lambda ()
              (setq-local imenu-auto-rescan t)
              (when (derived-mode-p 'prog-mode)
                (setq-local imenu-sort-function #'imenu--sort-by-name)))))

;; --- macOS 전용: GUI일 때만 ---
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
