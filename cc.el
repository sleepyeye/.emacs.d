;;;###autoload
(defconst sleepy/c++-ts-mode--override-indent-styles 
	'((((node-is "access_specifier") parent-bol 0)
		 ;; Indent the body of namespace definitions.
		 ((parent-is "declaration_list") parent-bol 0))
		((parent-is "if_statement") parent-bol 0)))


;;;###autoload
(defun sleepy/c-ts-mode--set-indent-style (mode)
	"Override default indent style based on k&r which is the default c-ts-mode indent style."
	(let ((style
				 (pcase mode
					 ('c (alist-get 'k&r (c-ts-mode--indent-styles mode)))
					 ('cpp (append sleepy/c++-ts-mode--override-indent-styles
												 (alist-get 'k&r (c-ts-mode--indent-styles mode)))))
				 ))
		`((,mode ,@style))))

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

(with-eval-after-load 'lsp
	(setq-default lsp-clients-clangd-args
								'("-j=4"
									"--background-index"
									;; "--clang-tidy"
									"--completion-style=detailed"
									"--header-insertion=never"
									"--header-insertion-decorators=0")))


;;; Currently we only need to guess indent offset in c-c++ modes
(elpaca-use-package dtrt-indent
	:hook (((c-mode c++-mode) . dtrt-indent-mode)
				 ((c-ts-mode c++-ts-mode) . dtrt-indent-mode))
	:config
	;; register c-ts-mode and  c++-ts-mode to dtrt-indent
	(add-to-list 'dtrt-indent-hook-mapping-list
							 '(c-ts-base-mode c/c++/java c-ts-mode-indent-offset))
	:custom
	;; dtrt-indent mode will automatically update the listed variables
	;; note all modes and variables should be registered in dtrt-indent-hook-mapping-list
	(dtrt-indent-hook-generic-mapping-list
	 '((evil-mode evil-shift-width)
		 (c-ts-base-mode c-ts-mode-indent-offset))))



;; (add-hook 'c-mode-hook
;; 					'(lambda ()
;; 						 (c-ts-mode)
;; 						 ;; (setq-local treesit-simple-indent-rules
;; 						 ;; 						 (sleepy/c-ts-mode--set-indent-style 'c)

;; 												 ))


;; (add-hook 'c++-mode-hook
;; 					'(lambda ()
;; 						 (c++-ts-mode)
;; 						 ;; (setq-local treesit-simple-indent-rules
;; 						 ;; 						 (sleepy/c-ts-mode--set-indent-style 'c++)

;; 												 ))


