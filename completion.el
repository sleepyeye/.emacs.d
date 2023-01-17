(elpaca-use-package vertico
  :demand t
  :init
  (vertico-mode)
  (setq vertico-count 20
	vertico-cycle t))

(elpaca-use-package consult
  :demand t
  :general
  (sleepy/buffer-keys
    "d" '(kill-current-buffer                :which-key  "kill current buffer")
    "r" '(revert-buffer                      :which-key  "revert buffer")
    "b" '(consult-buffer                     :which-key  "buffer list")
    "B" '(consult-buffer-other-window        :which-key  "buffer list other window"))
  (sleepy/search-keys
    "s" '(consult-line                       :which-key  "line")
    "d" '(consult-ripgrep                    :which-key  "rg")))

(elpaca-use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(elpaca-use-package marginalia
  :demand t
  :init
  (marginalia-mode))

(elpaca-use-package corfu
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 2)
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

