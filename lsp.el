(elpaca-use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (add-hook 'lsp-mode-hook #'evil-normalize-keymaps)
  :hook ((latex-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :general
  (general-evil-define-key '(normal) lsp-mode-map
    "gd" #'lsp-find-definition
    "gD" #'lsp-find-references)
  :commands lsp)

;; (elpaca-use-package consult-lsp
;;   :defer t
;;   :hook lsp-mode)


