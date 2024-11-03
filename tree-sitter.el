(use-package tree-sitter
  :defer 2
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(simpc-mode . cpp)))

(use-package tree-sitter-langs
  :defer t
  :after tree-sitter)
