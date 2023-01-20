(elpaca-use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (add-hook 'lsp-mode-hook #'evil-normalize-keymaps)
  :hook ((latex-mode . lsp)
	 (python-mode . lsp)
	 (python-ts-mode . lsp)
	 (c-mode . lsp)
	 (c-ts-mode . lsp)
	 (c++-mode . lsp)
	 (c++-ts-mode . lsp)
	 (cmake-ts-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :general
  (general-evil-define-key '(normal) lsp-mode-map
    "gd" #'lsp-find-definition
    "gD" #'lsp-find-references)
  :commands (lsp lsp-deferred))

;; (elpaca-use-package consult-lsp
;;   :defer t
;;   :hook lsp-mode)


