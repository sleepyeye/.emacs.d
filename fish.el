(elpaca-use-package fish-mode
  :mode (rx ".fish" eol)
  :hook
  (fish-mode . (lambda () (add-hook 'before-save-hook 'fish_indent-before-save))))

