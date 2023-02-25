(elpaca-use-package doom-modeline
  :demand t
	:init
	(setq doom-modeline-height 25)
  :config
  (column-number-mode 1)
  (doom-modeline-mode)
  :custom
  (doom-modeline-icon t "Show icons in the modeline"))


(elpaca-use-package modus-themes
  :demand t
  :config
  (load-theme 'modus-operandi t))

(elpaca-use-package fontaine
  :demand t
  :after modus-themes
  :init
  (add-hook 'modus-themes-after-load-theme-hook #'fontaine-apply-current-preset)
  :config
  (setq fontaine-presets
	'((regular
	   :default-family "Fira Code"
	   :default-height 140 
	   :default-weight regular
	   :fixed-pitch-family "Fira Code"
	   :fixed-pitch-serif-family "IBM Plex Serif"
	   :variable-pitch-family "IBM Plex Sans"
	   :italic-family "JuliaMono"
	   :italic-slant italic
	   :bold-weight bold
	   :line-spacing 1)))
  (fontaine-set-preset 'regular))


(elpaca-use-package pulsar
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

(elpaca-use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode))

;; (elpaca-use-package doom-themes
;;   :demand t
;;   :config
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one t)
;;   (doom-themes-org-config))

(elpaca-use-package solaire-mode
  :config
  (solaire-global-mode +1)
  (add-hook 'ediff-prepare-buffer-hook #'solaire-mode))
  
(setq confirm-kill-emacs 'y-or-n-p)
(setq display-line-numbers t
      display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(setq text-quoting-style 'curve)

