(use-package treesit
    :ensure nil
    :init
	(setq treesit-language-source-alist
		  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
			(c "https://github.com/tree-sitter/tree-sitter-c")
			(cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4")
			(css "https://github.com/tree-sitter/tree-sitter-css")
			(cmake "https://github.com/uyha/tree-sitter-cmake")
			(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
			(elisp "https://github.com/Wilfred/tree-sitter-elisp")
			(html "https://github.com/tree-sitter/tree-sitter-html")
			(javascript "https://github.com/tree-sitter/tree-sitter-javascript")
			(json "https://github.com/tree-sitter/tree-sitter-json")
			(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src")
			(org "https://github.com/milisims/tree-sitter-org")
			(python "https://github.com/tree-sitter/tree-sitter-python")
			(typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
			(tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
			(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
			(toml "https://github.com/tree-sitter/tree-sitter-toml"))

          major-mode-remap-alist
          '((c-mode          . c-ts-mode)
            (c++-mode        . c++-ts-mode)
            (c-or-c++-mode   . c-or-c++-ts-mode)
            (python-mode     . python-ts-mode)
            (cmake-mode      . cmake-ts-mode)
            (conf-toml-mode  . toml-ts-mode)
			(js-mode         . js-ts-mode)
            (js-json-mode    . json-ts-mode)
            (sh-mode         . bash-ts-mode)
            (typescript-mode . typescript-ts-mode))
          treesit-font-lock-level 4)

    (add-to-list 'auto-mode-alist '("CMakeLists\\'" . cmake-ts-mode))
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode)))
