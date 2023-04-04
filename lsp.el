(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (add-hook 'lsp-mode-hook #'evil-normalize-keymaps)
  :hook ((latex-mode . lsp-deferred)
		 (python-mode . lsp-deferred)
		 (python-ts-mode . lsp-deferred)
		 (c-mode . lsp-deferred)
		 (c-ts-mode . lsp-deferred)
		 (c++-mode . lsp-deferred)
		 (c++-ts-mode . lsp-deferred)
		 (cmake-mode . lsp-deferred)
		 (cmake-ts-mode . lsp-deferred)
		 (lsp-mode . lsp-enable-which-key-integration))
  :config

  (general-evil-define-key '(normal) lsp-mode-map
	"gd" #'lsp-find-definition
	"gD" #'lsp-find-references
	"g=" #'lsp-format-buffer)

  (general-evil-define-key '(normal visual motion) lsp-mode-map
	"=" #'lsp-format-region)


  (sleepy/leader-def "cr" #'lsp-rename)
  
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-completion-provider :none)
  (defun corfu-lsp-setup ()
	(setq-local completion-styles '(orderless)
				completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
  (setq lsp-diagnostics-provider :none)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-modeline-diagnostics-enable nil)

  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  ;; (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-idle-delay 0.5))

;; (elpaca-use-package consult-lsp
;;   :defer t
;;   :hook lsp-mode)

