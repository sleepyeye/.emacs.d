
(elpaca-use-package doom-modeline
  :demand t
  :config
  (column-number-mode 1)
  (doom-modeline-mode)
  :custom
  (doom-modeline-icon t "Show icons in the modeline"))


;; theme
(load-theme 'modus-operandi)            ; Light theme

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
  (pulsar-global-mode 1))

(elpaca-use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode))
  
