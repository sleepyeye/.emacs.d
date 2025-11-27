;;; eglot.el --- LSP configuration with Eglot -*- lexical-binding: t; -*-

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook
  ;; Auto-start eglot for configured modes
  ((python-mode python-ts-mode) . eglot-ensure)
  ((c-mode c++-mode c-ts-mode c++-ts-mode) . eglot-ensure)
  (LaTeX-mode . eglot-ensure)
  ((cmake-mode cmake-ts-mode) . eglot-ensure)
  :custom
  (eglot-report-progress nil)
  (eglot-events-buffer-size 0)
  :init
  (setq eglot-autoshutdown t
	eglot-sync-connect 0
	eglot-workspace-configuration
	'(:basedpyright
	  (:typeCheckingMode "standard"
			     :disableOrganizeImports t
			     :analysis
			     (:inlayHints (:callArgumentNames :json-false)
					  :diagnosticSeverityOverrides
					  (:reportCallIssue "none"
							    :reportUnusedCallResult "none"
							    :reportGeneralTypeIssues "none"
							    :reportArgumentType "none")
					  :useLibraryCodeForTypes t
					  :autoImportCompletions :json-false
					  :stubPath ["./" "./typings"]
					  :diagnosticMode "openFilesOnly"  ; was "workspace" - too expensive
					  :autoSearchPaths t))))

  :config

  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)

  ;; Performance: reduce server communication frequency
  (setq eglot-send-changes-idle-time 0.5)  ; Wait 0.5s before sending changes

  ;; Eglot behavior settings
  (setq eglot-extend-to-xref t
        eglot-ignored-server-capabilities '(:inlayHintProvider :foldingRangeProvider))

  ;; Completion integration with orderless
  (setq completion-category-overrides
        '((file (styles partial-completion))
          (eglot (styles orderless basic))))

  ;; Cape integration for better completion
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; Language server program configurations
  (add-to-list 'eglot-server-programs
    '((c-mode c-ts-mode c++-mode c++-ts-mode)
      . ("clangd" "-j=2" "--log=error" "--completion-style=bundled"
         "--background-index" "--header-insertion=never" "--header-insertion-decorators=0")))
  (add-to-list 'eglot-server-programs '((LaTeX-mode) . ("texlab")))
  (add-to-list 'eglot-server-programs
		   '((python-mode python-ts-mode)
		     . ("basedpyright-langserver" "--stdio"))))

(use-package consult-eglot
  :after eglot
  :config
  (when (fboundp 'sleepy/leader-def)
    (sleepy/leader-def "cs" #'consult-eglot-symbols)))

;; Additional eglot keybindings for common LSP operations
(with-eval-after-load 'eglot
  (with-eval-after-load 'general
    (when (fboundp 'sleepy/leader-def)
      (sleepy/leader-def
        ;; Code actions and refactoring
        "c a" '(eglot-code-actions :which-key "code actions")
        "c r" '(eglot-rename :which-key "rename symbol")
        "c f" '(eglot-format :which-key "format buffer/region")
        "c o" '(eglot-code-action-organize-imports :which-key "organize imports")
        ;; Navigation
        "c d" '(xref-find-definitions :which-key "find definitions")
        "c D" '(xref-find-references :which-key "find references")
        "c i" '(eglot-find-implementation :which-key "find implementation")
        "c t" '(eglot-find-typeDefinition :which-key "find type definition")
        ;; Documentation and help
        "c h" '(eldoc-doc-buffer :which-key "show documentation")
        ;; Server management
        "c R" '(eglot-reconnect :which-key "restart LSP server")
        "c S" '(eglot-shutdown :which-key "shutdown LSP server")))))

(use-package eglot-booster
  :ensure (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode 1))

;;; eglot.el ends here
