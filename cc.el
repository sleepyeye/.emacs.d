(defun sleepy/cc-capf ()
  (setq-local completion-at-point-functions
			  (list (cape-capf-super
					 #'tempel-complete
					 ;; #'cape-keyword
					 #'eglot-completion-at-point))))


;;; Why not (add-hook 'c-mode-hook #'sleepy/cc-capf)?
;;; I realized when I turn on eglot, it automatically prepend 'eglot-completion-at-point to completion-at-point-functions
;;; I want to manage my capf per mode explicitly.
;;; Hence I ends-up this ugly implementation
(add-hook 'c-mode-hook (lambda () (add-hook 'eglot-managed-mode-hook #'sleepy/cc-capf)))
(add-hook 'c++-mode-hook (lambda () (add-hook 'eglot-managed-mode-hook #'sleepy/cc-capf)))
(add-hook 'cc-mode-hook (lambda () (add-hook 'eglot-managed-mode-hook #'sleepy/cc-capf)))
(add-hook 'simpc-mode-hook (lambda () (add-hook 'eglot-managed-mode-hook #'sleepy/cc-capf)))


(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cc-mode :elpaca nil :disabled t)
(use-package c-mode :elpaca nil :disabled t)
(use-package c++-mode :elpaca nil :disabled t)

(use-package simpc-mode
  :init
  (add-to-list 'major-mode-remap-alist '(c-mode . simpc-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . simpc-mode))
  :load-path local-package-directory)

(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package modern-cpp-font-lock
  :load-path local-package-directory
  :config
  (modern-c++-font-lock-global-mode))

(use-package clang-format+
  :hook ((c-mode-hook . clang-format+-mode)
		 (c++-mode-hook . clang-format+-mode)
		 (simpc-mode . clang-format+-mode)))
