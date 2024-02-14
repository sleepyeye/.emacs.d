(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :config
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  (setq completion-category-defaults nil)
  (setq eglot-extend-to-xref t)

  ;; setup language servers for each language
  (add-to-list 'eglot-server-programs
			   '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
			   '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
			   '((tex-mode latex-mode LaTex-mode) . ("texlab")))
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
   (latex-mode . eglot-ensure)
   (tex-mode . eglot-ensure)
   (LaTeX-mode . eglot-ensure)
   (cmake-mode . eglot-ensure)
   (cmake-ts-mode . eglot-ensure)
   ;; (markdown-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)
   (c++-ts-mode . eglot-ensure)))

(use-package consult-eglot
  :after eglot
  :general
  (sleepy/leader-def
    "cs" 'consult-eglot-symbols))
