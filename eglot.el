(use-feature eglot
  :commands (eglot eglot-ensure)
  :init
  (add-to-list 'eglot-stay-out-of 'flymake)
  :config
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  (setq completion-category-defaults nil)
  (setq eglot-extend-to-xref t)

  (add-to-list 'eglot-server-programs
			   '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("ccls")))

  ;; (add-to-list 'eglot-server-programs
  ;; 			   '((c-mode c-ts-mode c++-mode c++-ts-mode)
  ;; 				 . ("clangd"
  ;; 					"-j=8"
  ;; 					"--log=error"
  ;; 					;; cause crash in macos
  ;; 					;; "--malloc-trim"
  ;; 					"--background-index"
  ;; 					"--clang-tidy"
  ;; 					"--cross-file-rename"
  ;; 					"--completion-style=detailed"
  ;; 					"--pch-storage=memory"
  ;; 					"--header-insertion=never"
  ;; 					"--header-insertion-decorators=0")))

  (add-to-list 'eglot-server-programs
			   '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))

  (general-evil-define-key '(normal) eglot-mode-map
	"gd" #'xref-find-definitions
	"gr" #'xref-find-references
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
