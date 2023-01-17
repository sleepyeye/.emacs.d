
(elpaca-use-package doom-modeline
  :demand t
  :config
  (column-number-mode 1)
  (doom-modeline-mode)
  :custom
  (doom-modeline-icon t "Show icons in the modeline"))


;; theme
(load-theme 'modus-operandi)            ; Light theme
