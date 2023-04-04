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
  
(setq confirm-kill-emacs 'y-or-n-p)
;; (setq display-line-numbers t
;;       display-line-numbers-type 'relative)
;; (global-display-line-numbers-mode)
(setq text-quoting-style 'curve)


;; shackle gives you the means to put an end to popped up buffers not behaving they way you’d like them to.
(use-package shackle
  :elpaca (shackle :depth nil)
  :commands (shackle-mode)
  :custom (shackle-rules '(("*Flycheck errors*"  :align below :size 0.15)
                           ("\\`\\*Flymake diagnostics.*?\\*\\'" :align below :size 0.15 :regexp t :same nil)
                           ("*accord*" :align below :size 0.20)
                           ("*padscape*" :align below :size 0.20)))
  :hook ((flycheck-mode global-flycheck-mode flymake-mode accord-mode padscape-mode) . shackle-mode))

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


;; (use-package popper
;;   :defer 2
;;   :bind (("C-`"   . popper-toggle-latest)
;; 		 ("M-`"   . popper-cycle)
;; 		 ("C-M-`" . popper-toggle-type))
;;   :init
;;   (setq popper-group-function #'popper-group-by-projectile) ; projectile projects
;;   (setq popper-reference-buffers
;; 		'("\\*Messages\\*"
;; 		  "Output\\*$"
;; 		  "\\*Warnings\\*"
;; 		  "\\*Async Shell Command\\*"
;; 		  help-mode
;; 		  compilation-mode))
;;   (popper-mode +1)
;;   (popper-echo-mode +1))


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
