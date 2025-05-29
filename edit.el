(use-package iedit)

(use-package ialign)

(use-package wgrep
  :config
  (setq wgrep-change-readonly-file t)
  (setq wgrep-auto-save-buffer t))


(use-package expand-region
  :bind (("M-=". er/expand-region)
		 ("M-+". er/expand-region)
		 ("M--". er/contract-region)))


(use-package tempel
  ;; Require trigger prefix before template name when completing.
  :config
  (setq tempel-trigger-prefix "<")
  ;; setup keymaps
  :bind (:map tempel-map
		 ("<tab>"     . #'tempel-next)
		 ("<backtap>" . #'tempel-previous)
		 ("C-["       . #'tempel-next)
		 ("C-]"       . #'tempel-previous)
		 ("C-g"       . #'tempel-abort)
		 ("C-k"       . #'tempel-kill)))
