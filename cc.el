(use-feature cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(elpaca-use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))


(elpaca-use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode t))

(elpaca-use-package google-c-style
  :hook ((c-mode c++-mode) . google-set-c-style)
  (c-mode-common . google-make-newline-indent))


(elpaca-use-package clang-format+
	:hook ((c-mode-hook . clang-format+-mode)
				 (c++-mode-hook . clang-format+-mode)))

(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

