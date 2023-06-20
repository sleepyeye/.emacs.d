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
