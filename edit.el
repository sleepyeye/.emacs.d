(use-package drag-stuff
  :demand t
  :diminish drag-stuff-mode
  :config
  (drag-stuff-global-mode t)
  (general-define-key
   :keymaps 'override
   "M-j" #'drag-stuff-down
   "M-k" #'drag-stuff-up
   "M-h" #'drag-stuff-left
   "M-l" #'drag-stuff-right))

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


;; Configure Tempel
(use-package tempel
  :defer 2
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<"))
