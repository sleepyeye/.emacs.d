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
  (setq which-key-frame-max-height 20)
  (setq which-key-side-window-location 'bottom
		which-key-enable-extended-define-key t
		which-key-sort-order #'which-key-key-order-alpha
		which-key-sort-uppercase-first nil
		which-key-add-column-padding 1
		which-key-max-display-columns nil
		which-key-min-display-lines 6
		which-key-side-window-slot -10
		which-key-side-window-max-width 0.25
		which-key-idle-delay 0.05
		which-key-max-description-length 25
		which-key-allow-imprecise-window-fit t)
  :diminish which-key-mode)


(use-package doom-modeline
  :init
  (setq doom-modeline-height 15)
  :config
  (column-number-mode 1)
  (doom-modeline-mode)
  :custom
  (doom-modeline-icon t "Show icons in the modeline"))

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
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi :no-confirm))


(use-package pulsar
  :defer 2
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
  :defer 2
  :config
  (global-hl-todo-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1)
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode))


;; (set-face-attribute 'fringe nil :background nil)
;; (set-face-attribute 'header-line nil :background nil :inherit 'default)
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)


(use-package popper
  :defer 2
  :after perspective
  :config
  ;; Group by projects
  (setq popper-group-function #'popper-group-by-projectile)
  (setq popper-mode-line nil)
  (setq popper-display-control t)
  (general-define-key
   "C-;" #'popper-toggle
   "C-'" #'popper-cycle)
  :init
  (setq popper-reference-buffers
		'(help-mode
		  compilation-mode
		  messages-mode
		  occur-mode
		  "\\*Messages\\*"
		  "^\\*Warnings\\*"
		  "^\\*Compile-Log\\*"
		  "^\\*Backtrace\\*"
		  "\\*Shell Command Output\\*"
		  "\\*Async Shell Command\\*"
		  "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
		  "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
		  "Output\\*$"
		  ))
  (setq popper-window-height 0.37)
  (popper-mode +1)
  (popper-echo-mode +1))

;; ;; FIXME
;; ;; Stolen from @xenodium's config
;; ;; Welcome buffer
;; (defun ar/show-welcome-buffer ()
;;   "Show *Welcome* buffer."
;;   (with-current-buffer (get-buffer-create "*Welcome*")
;; 	(setq truncate-lines t)
;; 	(let* ((buffer-read-only)
;; 		   (image-path "~/.emacs.d/emacs.png")
;; 		   (image (create-image image-path))
;; 		   (size (image-size image))
;; 		   (height (cdr size))
;; 		   (width (car size))
;; 		   (top-margin (floor (/ (- (window-height) height) 2)))
;; 		   (left-margin (floor (/ (- (window-width) width) 2)))
;; 		   (prompt-title "Welcome to Emacs!"))
;; 	  (erase-buffer)
;; 	  (setq mode-line-format nil)
;; 	  (goto-char (point-min))
;; 	  (insert (make-string top-margin ?\n ))
;; 	  (insert (make-string left-margin ?\ ))
;; 	  (insert-image image)
;; 	  (insert "\n\n\n")
;; 	  (insert (make-string (floor (/ (- (window-width) (string-width prompt-title)) 2)) ?\ ))
;; 	  (insert prompt-title))
;; 	(setq cursor-type nil)
;; 	(read-only-mode +1)
;; 	(switch-to-buffer (current-buffer))
;; 	(local-set-key (kbd "q") 'kill-this-buffer)))
;; (setq initial-scratch-message nil)
;; (setq inhibit-startup-screen t)
;; (when (< (length command-line-args) 2)
;;   (add-hook 'emacs-startup-hook (lambda ()
;; 								  (when (display-graphic-p)
;; 									(ar/show-welcome-buffer)))))

(use-package frame
  :ensure nil
  :defer t
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
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
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

(use-package spacious-padding
  :config
  (spacious-padding-mode 1))
