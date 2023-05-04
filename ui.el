(use-package doom-modeline
  :demand t
  :init
  (setq doom-modeline-height 25)
  :config
  (column-number-mode 1)
  (doom-modeline-mode)
  :custom
  (doom-modeline-icon t "Show icons in the modeline"))


;; (use-package modus-themes
;;   :demand t
;; 	:init
;; 	(setq modus-themes-italic-constructs t
;; 				modus-themes-bold-constructs t
;; 				modus-themes-mixed-fonts t
;; 				modus-themes-variable-pitch-ui t
;; 				modus-themes-disable-other-themes t)
;; 	;; Color customizations
;; 	(setq modus-themes-prompts '(bold))
;; 	(setq modus-themes-completions nil)
;; 	(setq modus-themes-org-blocks 'gray-background)
;;   :config
;;   (load-theme 'modus-operandi t))

(use-package doom-themes
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

(use-package pulsar
  :init
  :hook
  ((consult-after-jump-hook . pulsar-recenter-top)
   (consult-after-jump-hook . pulsar-reveal-entry))
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1)
  :custom
  (pulsar-pulse-functions '(recenter-top-bottom
							move-to-window-line-top-bottom
							reposition-window
							bookmark-jump
							other-window
							delete-window
							delete-other-windows
							forward-page
							backward-page
							scroll-up-command
							scroll-down-command
							evil-window-right
							evil-window-left
							evil-window-up
							evil-window-down))
  (pulsar-face 'pulsar-magenta)
  (pulsar-delay 0.055))

(use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1)
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode))


;; ;; shackle gives you the means to put an end to popped up buffers not behaving they way you’d like them to.
;; (use-package shackle
;;   :elpaca (shackle :depth nil)
;;   :commands (shackle-mode)
;;   :custom (shackle-rules '(("*Flycheck errors*"  :align below :size 0.15)
;; 						   ("\\`\\*Flymake diagnostics.*?\\*\\'" :align below :size 0.15 :regexp t :same nil)
;; 						   ("*accord*" :align below :size 0.20)
;; 						   ("*padscape*" :align below :size 0.20)))
;;   :hook ((flycheck-mode global-flycheck-mode flymake-mode accord-mode padscape-mode) . shackle-mode))

;; (dolist (face '(window-divider
;; 				window-divider-first-pixel
;; 				window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))
(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil) ;; Otherwise shows a corner icon on the edge
(setq-default indicate-empty-lines nil) ;; Otherwise there are weird fringes on blank lines

(set-face-attribute 'fringe nil :background nil)
(set-face-attribute 'header-line nil :background nil :inherit 'default)

;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)


(use-package popper
  :defer 2
  :after perspective
  :config
  (setq popper-group-function #'popper-group-by-perspective) ; projectile projects
  :init
  (setq popper-reference-buffers
		'("\\*Messages\\*"
		  "Output\\*$"
		  "\\*Warnings\\*"
		  "\\*Async Shell Command\\*"
		  help-mode
		  compilation-mode
		  "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
		  "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
		  "^\\*chatgpt.*\\*$" chatgpt-shell-mode ;chatgpt-shell as a popup
		  ))
  (setq popper-window-height 0.37)
  (popper-mode +1)
  (popper-echo-mode +1)

  :config
  (general-define-key
   "C-;" #'popper-toggle-latest
   "C-'" #'popper-cycle))


;; ;; Uses simpleclip
;; (defun jib/copy-whole-buffer-to-clipboard ()
;;   "Copy entire buffer to clipboard"
;;   (interactive)
;;   (save-excursion
;; 	(mark-whole-buffer)
;; 	(simpleclip-copy (point-min) (point-max))
;; 	(deactivate-mark))
;;   (message "Copied entire buffer to clipboard"))

;; (defun jib/emacs-clipboard-to-system-clipboard ()
;;   "Set system clipboard to contents of Emacs kill ring."
;;   (interactive)
;;   (simpleclip-set-contents (substring-no-properties (nth 0 kill-ring))))

;; (defun jib/system-clipboard-to-emacs-clipboard ()
;;   "Set Emacs kill ring to contents of system clipboard."
;;   (interactive)
;;   (kill-new (simpleclip-get-contents)))

;; FIXME
;; Stolen from @xenodium's config
(defun ar/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
	(setq truncate-lines t)
	(let* ((buffer-read-only)
		   (image-path "~/.emacs.d/emacs.png")
		   (image (create-image image-path))
		   (size (image-size image))
		   (height (cdr size))
		   (width (car size))
		   (top-margin (floor (/ (- (window-height) height) 2)))
		   (left-margin (floor (/ (- (window-width) width) 2)))
		   (prompt-title "Welcome to Emacs!"))
	  (erase-buffer)
	  (setq mode-line-format nil)
	  (goto-char (point-min))
	  (insert (make-string top-margin ?\n ))
	  (insert (make-string left-margin ?\ ))
	  (insert-image image)
	  (insert "\n\n\n")
	  (insert (make-string (floor (/ (- (window-width) (string-width prompt-title)) 2)) ?\ ))
	  (insert prompt-title))
	(setq cursor-type nil)
	(read-only-mode +1)
	(switch-to-buffer (current-buffer))
	(local-set-key (kbd "q") 'kill-this-buffer)))

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

(when (< (length command-line-args) 2)
  (add-hook 'emacs-startup-hook (lambda ()
								  (when (display-graphic-p)
									(ar/show-welcome-buffer)))))
