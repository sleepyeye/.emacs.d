(elpaca-use-package cmake-mode
	:hook (cmake-mode . lsp-deferred)
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(elpaca-use-package cmake-font-lock
  :after cmake-mode
  :config (cmake-font-lock-activate))

(elpaca-use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode t))

(elpaca-use-package google-c-style
	:hook (((c-mode c++-mode) . google-set-c-style)
				 (c-mode-common . google-make-newline-indent)))

(elpaca-use-package clang-format+)

(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)



(defconst sleepy/c-ts-mode--override-indent-styles
	'((match "while" "do_statement") parent 0))

(defun sleepy/c-ts-mode--set-indent-style (mode)
	"Override default indent style based on k&r which is the default c-ts-mode indent style."
	(let ((style (push sleepy/c-ts-mode--override-indent-styles
										 (alist-get 'k&r (c-ts-mode--indent-styles mode)))))
		`((,mode ,@style))))

;; ,@(when (eq mode 'cpp)
;;     '(((node-is "access_specifier") parent-bol 0)
;;       ;; Indent the body of namespace definitions.
;;       ((parent-is "declaration_list") parent-bol c-ts-mode-indent-offset)))

;; (setq-local treesit-simple-indent-rules
;; 						(sleepy/c-ts-mode--set-indent-style 'cpp))

;; (setq-local treesit-simple-indent-rules
;; 						(c-ts-mode--set-indent-style 'cpp))
	
