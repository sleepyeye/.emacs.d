
(use-package eglot
  :elpaca nil
  :commands (eglot eglot-ensure)
  :config
  (setq completion-category-defaults nil)
  (setq eglot-extend-to-xref t)
  (add-to-list 'eglot-server-programs
			   '((c-mode c-ts-mode c++-mode c++-ts-mode)
				 . ("clangd"
					"-j=8"
					"--log=error"
					;; cause crash in macos
					;; "--malloc-trim"
					"--background-index"
					"--clang-tidy"
					"--cross-file-rename"
					"--completion-style=detailed"
					"--pch-storage=memory"
					"--header-insertion=never"
					"--header-insertion-decorators=0")))

  (general-evil-define-key '(normal) eglot-mode-map
	"gd" #'xref-find-definitions
	"gD" #'xref-find-references
	"ga" #'eglot-code-actions
	"g=" #'eglot-format-buffer)
  (general-evil-define-key '(normal visual motion) eglot-mode-map
	"=" #'eglot-format)
  (sleepy/leader-def "cr" #'eglot-rename)
  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   (cmake-mode . eglot-ensure)
   (cmake-ts-mode . eglot-ensure)
   (markdown-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)
   (c++-ts-mode . eglot-ensure)))

