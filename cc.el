;;; cc.el --- cc.el -*- no-byte-compile: t; lexical-binding: t; -*-

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


(use-package cmake-mode
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))


(use-package cmake-font-lock
  :defer t
  :commands cmake-font-lock-activate
  :after cmake-mode
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode t))

(use-package clang-format+
  :defer t
  :commands clang-format+-mode
  :hook ((c-mode-hook . clang-format+-mode)
		 (c++-mode-hook . clang-format+-mode)))
