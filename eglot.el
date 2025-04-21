(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :custom
  (eglot-report-progress nil)  ; Prevent minibuffer spam
  (eglot-events-buffer-size 0)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  (setq completion-category-defaults nil)
  (setq eglot-extend-to-xref t)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider :foldingRangeProvider))

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; setup language servers for each language
  (add-to-list 'eglot-server-programs
			   '((simpc-mode c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd"
																	   "-j=2"
																	   "--log=error"
																	   ;; "--completion-style=detailed"
																	   "--completion-style=bundled" ;; more simpler style
																	   "--background-index"
																	   "--header-insertion=never"
																	   "--header-insertion-decorators=0")))
  ;; (add-to-list 'eglot-server-programs
  ;; 			   '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
			   '((LaTeX-mode) . ("texlab")))
  :general
  (sleepy/leader-def
	"ca" #'eglot-code-action)
  (:keymaps 'eglot-mode-map :states 'normal
			"ga" #'eglot-code-action
			"ga" #'eglot-format-buffer
			"g=" #'eglot-format)
  (:keymaps 'eglot-mode-map :states 'visual
			"g=" #'eglot-format)

  (sleepy/leader-def "cr" #'eglot-rename)

  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   ;; (latex-mode . eglot-ensure)
   ;; (tex-mode . eglot-ensure)
   (LaTeX-mode . eglot-ensure)
   (cmake-mode . eglot-ensure)
   (cmake-ts-mode . eglot-ensure)
   ;; (markdown-mode . eglot-ensure)
   ;; (c-mode . eglot-ensure)
   ;; (c++-mode . eglot-ensure)
   ;; (c-ts-mode . eglot-ensure)
   ;; (c++-ts-mode . eglot-ensure)
   (simpc-mode . eglot-ensure)
  ))

(use-package consult-eglot
  :after eglot
  :general
  (sleepy/leader-def
    "cs" 'consult-eglot-symbols))

(use-package eglot-booster
  :ensure (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))
