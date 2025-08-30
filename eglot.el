(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :custom
  (eglot-report-progress nil)
  (eglot-events-buffer-size 0)
  :init
  (setq eglot-autoshutdown t
		eglot-sync-connect 0
		;; ✅ 올바른 구조로 수정
		eglot-workspace-configuration
		'(:basedpyright
		  (:typeCheckingMode "standard"
							 :disableOrganizeImports t
							 :analysis
							 (:inlayHints (:callArgumentNames :json-false)
										  :diagnosticSeverityOverrides
										  (:reportCallIssue "none"
															:reportUnusedCallResult "none"
															:reportGeneralTypeIssues "none"
															:reportArgumentType "none")
										  :useLibraryCodeForTypes t
										  :autoImportCompletions :json-false
										  :stubPath ["./" "./typings"]
										  :diagnosticMode "workspace"
										  :autoSearchPaths t))))

  :config

  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)

  (setq completion-category-defaults nil
        eglot-extend-to-xref t
        eglot-ignored-server-capabilities '(:inlayHintProvider :foldingRangeProvider))

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; C/C++/LaTeX 서버
  (add-to-list 'eglot-server-programs
    '((c-mode c-ts-mode c++-mode c++-ts-mode)
      . ("clangd" "-j=2" "--log=error" "--completion-style=bundled"
         "--background-index" "--header-insertion=never" "--header-insertion-decorators=0")))
  (add-to-list 'eglot-server-programs '((LaTeX-mode) . ("texlab")))
  (add-to-list 'eglot-server-programs
			   '((python-mode python-ts-mode)
				 . ("basedpyright-langserver" "--stdio")))

  ;; 키바인딩(중복/오타 정리)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (boundp 'evil-normal-state-map)
                (with-eval-after-load 'evil
                  (evil-normalize-keymaps)))
              ;; leader
              (when (fboundp 'sleepy/leader-def)
                (sleepy/leader-def
                  "ca" #'eglot-code-action
                  "cr" #'eglot-rename))
              ;; normal/visual (중복된 "ga" 정리)
              (let ((map eglot-mode-map))
                (define-key map (kbd "g=") #'eglot-format)         ; normal
                (define-key map (kbd "ga") #'eglot-code-action)
                ;; visual에선 region format
                )))
  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   (LaTeX-mode . eglot-ensure)
   (cmake-mode . eglot-ensure)
   (cmake-ts-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)
   (c++-ts-mode . eglot-ensure)))

(use-package consult-eglot
  :after eglot
  :config
  (when (fboundp 'sleepy/leader-def)
    (sleepy/leader-def "cs" #'consult-eglot-symbols)))

(use-package eglot-booster
  :ensure (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode 1))
