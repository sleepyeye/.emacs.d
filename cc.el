(elpaca-use-package cmake-font-lock)

(elpaca-use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode t))


(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

