(use-package vterm
  :ensure t
  :config
  (setq vterm-shell (executable-find "fish")
        vterm-max-scrollback 10000))


;; TODO eshell
