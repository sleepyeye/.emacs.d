
(elpaca-use-package which-key
  :demand t
  :init
  (setq which-key-enable-extended-define-key t)
  :config
  (which-key-setup-side-window-bottom)
  ;; max width of which-key frame: number of columns (an integer)
  (setq which-key-frame-max-width 60)
  ;; max height of which-key frame: number of lines (an integer)
  (setq which-key-frame-max-height 20)
  (which-key-mode)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.25)
  (which-key-idle-delay 0.05)
  :diminish which-key-mode)
