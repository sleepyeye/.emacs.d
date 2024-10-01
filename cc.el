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

;; (use-package cc-mode
;;   :ensure nil
;;   :config
;;   (sleepy/leader-def
;; 	:keymaps 'c-mode-base-map
;; 	"SPC" #'projectile-find-other-file
;; 	"pc"  #'projectile-compile-project
;; 	"pC"  #'projectile-configure-project)
;; 	;;; from emacs wiki
;;   (c-add-style "microsoft"
;; 			   '("stroustrup"
;; 				 (c-offsets-alist
;; 				  (innamespace . -)
;; 				  (inline-open . 0)
;; 				  (inher-cont . c-lineup-multi-inher)
;; 				  (arglist-cont-nonempty . +)
;; 				  (template-args-cont . +)))))

;; (use-package google-c-style
;;   :hook ((c-mode c++-mode c-ts-mode c++-ts-mode) . (lambda ()
;; 													 (google-set-c-style)
;; 													 (google-make-newline-indent)
;; 													 (setq c-basic-offset 4))))

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
  ;; (setq modern-c++-font-lock-keywords t)
  ;; (setq modern-c++-font-lock-stl-cstdint nil)
  (modern-c++-font-lock-global-mode t))

(use-package clang-format+
  :hook ((c-mode-hook . clang-format+-mode)
		 (c++-mode-hook . clang-format+-mode)
		 (simpc-mode . clang-format+-mode)))

;; ;;; Currently we only need to guess indent offset in c-c++ modes
;; (use-package dtrt-indent
;;   :hook ((c-mode . dtrt-indent-mode)
;; 		 (c++-mode . dtrt-indent-mode))
;;   :config
;;   ;; register c-ts-mode and  c++-ts-mode to dtrt-indent
;;   (add-to-list 'dtrt-indent-hook-mapping-list
;; 			   '(c-ts-base-mode c/c++/java c-ts-mode-indent-offset))
;;   :custom
;;   ;; dtrt-indent mode will automatically update the listed variables
;;   ;; note all modes and variables should be registered in dtrt-indent-hook-mapping-list
;;   (dtrt-indent-hook-generic-mapping-list
;;    '((evil-mode evil-shift-width)
;; 	 (c-ts-base-mode c-ts-mode-indent-offset))))
