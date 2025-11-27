;;; ui.el --- ui.el -*- no-byte-compile: t; lexical-binding: t; -*-

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-minibuffer)
  (which-key-setup-side-window-bottom)
  ;; max width of which-key frame: number of columns (an integer)
  (setq which-key-frame-max-width 60)
  ;; max height of which-key frame: number of lines (an integer)
  (setq which-key-frame-max-height 22)
  (setq which-key-side-window-location 'bottom
		which-key-enable-extended-define-key t
		which-key-sort-order #'which-key-key-order-alpha
		which-key-sort-uppercase-first nil
		which-key-add-column-padding 1
		which-key-max-display-columns nil
		which-key-min-display-lines 6
		which-key-side-window-slot -10
		which-key-side-window-max-width 0.25
		which-key-idle-delay 0.2
		which-key-max-description-length 25
		which-key-allow-imprecise-window-fit t)
  :diminish which-key-mode)

;; Modern modeline with icons, LSP info, and Git status
(use-package doom-modeline
  :disabled t
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  ;; Performance: reduce update frequency
  (setq doom-modeline-bar-width 3
        doom-modeline-height 25
        doom-modeline-window-width-limit 85
        doom-modeline-project-detection 'projectile
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 12
        doom-modeline-persp-name t
        doom-modeline-lsp t
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-irc nil
        doom-modeline-env-version t)
  :config
  ;; Nerd icons required for doom-modeline
  (require 'nerd-icons))

(use-package doom-themes
  :disabled t
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
		doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-snazzy t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; For packaged versions which must use `require'.
(use-package modus-themes
  :ensure t
  :config

  (setq modus-themes-common-palette-overrides
		'(;;; remove overline from headings
		  (overline-heading-0 unspecified)
		  (overline-heading-1 unspecified)
		  (overline-heading-2 unspecified)
		  (overline-heading-3 unspecified)
		  (overline-heading-4 unspecified)
		  (overline-heading-5 unspecified)
		  (overline-heading-6 unspecified)
		  (overline-heading-7 unspecified)
		  (overline-heading-8 unspecified)
		  ;;; adjust mode-line setup
		  (bg-mode-line-active bg-blue-subtle)
		  (fg-mode-line-active fg-main)
		  (border-mode-line-active blue-intense)))

  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi :no-confirm))


(use-package pulsar
  :hook
  ((consult-after-jump-hook . pulsar-recenter-top)
   (consult-after-jump-hook . pulsar-reveal-entry))
  :init
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (setq pulsar-pulse-functions '(recenter-top-bottom
								 move-to-window-line-top-bottom
								 reposition-window
								 bookmark-jump
								 other-window
								 delete-window
								 delete-other-windows
								 forward-page
								 backward-page
								 xref-find-references
								 xref-find-definitions
								 evil-jump-backward
								 scroll-up-command
								 scroll-down-command
								 evil-forward-paragraph
								 evil-backward-paragraph
								 evil-scroll-up
								 evil-scroll-down
								 evil-window-right
								 evil-window-left
								 evil-window-up
								 evil-window-down))
  :config
  (pulsar-global-mode 1))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1)
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode))

(use-package popper
  :config
  ;; Group by projects
  (setq popper-group-function #'popper-group-by-projectile)
  (setq popper-mode-line nil)
  (setq popper-display-control t)
  (general-define-key
   "C-M-'" #'popper-toggle-type
   "C-`" #'popper-toggle
   "C-'" #'popper-cycle)

  ;; Prevent which-key from showing when in a popper window
  (defun sleepy/which-key-inhibit-in-popper (orig-fun &rest args)
    "Inhibit which-key popup when current window is a popper."
    (unless (and (bound-and-true-p popper-mode)
                 (popper-popup-p (current-buffer)))
      (apply orig-fun args)))

  (advice-add 'which-key--show-popup :around #'sleepy/which-key-inhibit-in-popper)

  :init
  (setq popper-reference-buffers
		'(help-mode
		  compilation-mode
		  messages-mode
		  occur-mode
		  "\\*Python\\*"
		  "\\*Messages\\*"
		  "^\\*Warnings\\*"
		  "^\\*Compile-Log\\*"
		  "^\\*Backtrace\\*"
		  "\\*Shell Command Output\\*"
		  "\\*Async Shell Command\\*"
		  "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
		  "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
		  "^\\*Claude Code IDE.*\\*$"    ;Claude Code IDE buffers
		  "Output\\*$"
		  ))
  (setq popper-window-height 20)
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package frame
  :ensure nil
  :config
  (setq frame-resize-pixelwise t)
  (set-frame-parameter nil 'internal-border-width 0)
  (set-frame-position (selected-frame) 30 60)
  (set-frame-size (selected-frame)
				  (- (nth 3 (assq 'geometry (car (display-monitor-attributes-list)))) 60 29)
				  (- (nth 4 (assq 'geometry (car (display-monitor-attributes-list)))) 60 60 30) t))

(use-package dired
  :ensure nil
  :commands dired
  :config
  ;; File operations
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-create-destination-dirs 'ask
        delete-by-moving-to-trash t)
  ;; Buffer management
  (setq dired-kill-when-opening-new-dired-buffer t
        dired-clean-confirm-killing-deleted-buffers nil)
  ;; Display options
  (setq dired-dwim-target t
        dired-free-space nil           ; Emacs 29.1
        dired-mouse-drag-files t       ; Emacs 29.1
        dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package breadcrumb
  :ensure (:host github :repo "joaotavora/breadcrumb")
  :hook ((prog-mode . breadcrumb-mode)
         (text-mode . breadcrumb-mode))
  :config
  ;; Customize the appearance
  (setq breadcrumb-project-crumb-separator " > "
        breadcrumb-imenu-crumb-separator " > "
        breadcrumb-project-max-length 30
        breadcrumb-imenu-max-length 30))

(use-package spacious-padding
  :config
  (spacious-padding-mode 1))

;; Zen/distraction-free writing mode
(use-package writeroom-mode
  :ensure t
  :commands writeroom-mode
  :init
  (setq writeroom-width 100
        writeroom-mode-line t
        writeroom-bottom-divider-width 0
        writeroom-fringes-outside-margins nil
        writeroom-maximize-window nil
        writeroom-fullscreen-effect 'maximized)
  :config
  ;; Better integration with other modes
  (with-eval-after-load 'general
    (when (fboundp 'sleepy/leader-def)
      (sleepy/leader-def
        "t z" '(writeroom-mode :which-key "zen mode")))))
