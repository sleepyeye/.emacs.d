(use-package doom-modeline
  :demand t
	:init
	(setq doom-modeline-height 25)
  :config
  (column-number-mode 1)
  (doom-modeline-mode)
  :custom
  (doom-modeline-icon t "Show icons in the modeline"))


(use-package modus-themes
  :demand t
	:init 
	(setq modus-themes-italic-constructs t
				modus-themes-bold-constructs t
				modus-themes-mixed-fonts t
				modus-themes-variable-pitch-ui t
				modus-themes-disable-other-themes t)
	;; Color customizations
	(setq modus-themes-prompts '(bold))
	(setq modus-themes-completions nil)
	(setq modus-themes-org-blocks 'gray-background)

  :config
  (load-theme 'modus-operandi t))

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
(setq display-line-numbers t
      display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
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
