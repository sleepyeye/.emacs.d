(use-package tree-sitter
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(simpc-mode . cpp)))

(use-package tree-sitter-langs
  :after tree-sitter)
