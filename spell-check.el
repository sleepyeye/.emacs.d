(use-package jinx
  :hook ((latex-mode . jinx-mode)
		 (org-mode . jinx-mode))
  :bind ([remap ispell-word] . jinx-correct))
