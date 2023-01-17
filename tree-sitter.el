(use-feature tree-sitter
  :config
  ;; This makes every node a link to a section of code
  ;; and this highlights the entire sub tree in your code
  (setq tree-sitter-debug-jump-buttons t
	tree-sitter-debug-highlight-jump-region t))

(elpaca-use-package evil-textobj-tree-sitter
  :after (tree-sitter evil)
  :demand t)

(elpaca-use-package tree-sitter-langs
  :demand t)
