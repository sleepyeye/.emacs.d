(use-package jinx
  :hook ((text-mode . jinx-mode)
		 (org-mode . jinx-mode))
  :bind ([remap ispell-word] . jinx-correct))
