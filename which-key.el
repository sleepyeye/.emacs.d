(use-package which-key
  :demand t
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  :config
  (which-key-setup-side-window-bottom)
  ;; max width of which-key frame: number of columns (an integer)
  (setq which-key-frame-max-width 60)
  ;; max height of which-key frame: number of lines (an integer)
  (setq which-key-frame-max-height 20)
  (setq which-key-enable-extended-define-key t)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.25)
  (which-key-idle-delay 0.05)
  :diminish which-key-mode)
