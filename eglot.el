(use-feature eglot
  :commands (eglot eglot-ensure)
  :config
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  (setq completion-category-defaults nil)
  (setq eglot-extend-to-xref t)

  ;; setup language servers for each language
  (add-to-list 'eglot-server-programs
			   '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("ccls")))
  (add-to-list 'eglot-server-programs
			   '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))

  (setq-default eglot-workspace-configuration
				'((:python .
						   (:venvPath (expand-file-name "~/miniforge3/envs/") :pythonPath (expand-file-name "~/miniforge3/bin/python")))
				  (:pyright .
							(:analysis (:useLibraryCodeForTypes t :autoImportCompletions t :typeCheckingMode "basic")))))

  (general-evil-define-key 'normal eglot-mode-map
	"ga" #'eglot-code-actions
	"g=" #'eglot-format-buffer)
  (general-evil-define-key '(normal visual motion) eglot-mode-map
	"=" #'eglot-format)
  (sleepy/leader-def "cr" #'eglot-rename)

  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   (latex-mode . eglot-ensure)
   (tex-mode . eglot-ensure)
   (cmake-mode . eglot-ensure)
   (cmake-ts-mode . eglot-ensure)
   (markdown-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)
   (c++-ts-mode . eglot-ensure)
   ))
