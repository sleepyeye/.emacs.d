(use-package rg
  :init
  (rg-enable-default-bindings)
  :config
  (advice-add 'wgrep-change-to-wgrep-mode :after
			  #'evil-normal-state)
  (advice-add 'wgrep-to-original-mode :after
			  #'evil-motion-state)

  (defvar rg-mode-map)
  (add-to-list 'evil-motion-state-modes 'rg-mode)
  (evil-add-hjkl-bindings rg-mode-map 'motion
	"e" #'wgrep-change-to-wgrep-mode
	"g" #'rg-recompile
	"t" #'rg-rerun-change-literal))

(use-package dumb-jump
  :commands dumb-jump-xref-activate
  :config
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
