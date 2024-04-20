(defun sleepy/cc-capf ()
  (setq-local completion-at-point-functions
			  (list (cape-capf-super
					 #'tempel-complete
					 #'cape-keyword
					 #'eglot-completion-at-point))))

;;; Why not (add-hook 'c-mode-hook #'sleepy/cc-capf)?
;;; I realized when I turn on eglot, it automatically prepend 'eglot-completion-at-point to completion-at-point-functions
;;; I want to manage my capf per mode explicitly.
;;; Hence I ends-up this ugly implementation
(add-hook 'c-mode-hook (lambda () (add-hook 'eglot-managed-mode-hook #'sleepy/cc-capf)))
(add-hook 'c++-mode-hook (lambda () (add-hook 'eglot-managed-mode-hook #'sleepy/cc-capf)))
(add-hook 'cc-mode-hook (lambda () (add-hook 'eglot-managed-mode-hook #'sleepy/cc-capf)))



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



(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package compile :ensure nil)

(use-package cc-mode
  :ensure nil
  :after projectile
  :config
  (defun sleepy/cmake-configure-project ()
	(interactive)
	(let ((root-dir (projectile-project-root))
		  (export-compile-command-option "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
		  (surppress-werror-option "--compile-no-warning-as-error")
		  (build-options '(("Release" . "-DCMAKE_BUILD_TYPE=Release")
						   ("Debug" . "-DCMAKE_BUILD_TYPE=Debug")
						   ("RelWithDebInfo" . "-DCMAKE_BUILD_TYPE=RelWithDebInfo")
						   ("MinSizeRel" . "-DCMAKE_BUILD_TYPE=MinSizeRel")))
		  (generator-options '(("Ninja" . "-G \"Ninja\"")
							   ("Make" . "-G \"Unix Makefiles\""))))
	  (let* ((selected-build-option (completing-read "Select build type: " build-options nil t))
			 (selected-generator-option (completing-read "Select generator type: " generator-options nil t))
			 (build-type (cdr (assoc selected-build-option build-options)))
			 (generator (cdr (assoc selected-generator-option generator-options)))
			 (build-dir (expand-file-name "build" root-dir))
			 (command (format "cmake -B %s -S %s %s %s %s" build-dir root-dir build-type generator export-compile-command-option)))
		(compile command))))

  (defun sleepy/cmake-build-project ()
	(interactive)
	(let* ((root-dir (projectile-project-root))
		   (build-dir (expand-file-name "build" root-dir))
		   (command (format "cmake --build %s" build-dir)))
	  (compile command)))

  (setq-local projectile-project-compilation-cmd #'sleepy/cmake-build-project)
  (setq-local projectile-project-configure-cmd #'sleepy/cmake-configure-project))

(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package modern-cpp-font-lock
  :config
  (modern-c++-font-lock-global-mode t))

(use-package clang-format+
  :hook ((c-mode-hook . clang-format+-mode)
		 (c++-mode-hook . clang-format+-mode)))

;;; Currently we only need to guess indent offset in c-c++ modes
(use-package dtrt-indent
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
