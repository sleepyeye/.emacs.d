(defun sleepy/tex-capf ()
  (setq-local completion-at-point-functions
			  (list (cape-capf-super
					 #'tempel-complete
					 #'eglot-completion-at-point
					 #'cape-file))))


(add-hook 'latex-mode-hook (lambda () (add-hook 'eglot-managed-mode-hook #'sleepy/tex-capf)))
(add-hook 'LaTeX-mode-hook (lambda () (add-hook 'eglot-managed-mode-hook #'sleepy/tex-capf)))

(use-package auctex
  :demand t
  :ensure (auctex :pre-build (("./autogen.sh")
							  ("./configure"
							   "--without-texmf-dir"
							   "--with-packagelispdir=./"
							   "--with-packagedatadir=./")
							  ("make"))
				  :build (:not elpaca--compile-info) ;; Make will take care of this step
				  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
				  :version (lambda (_) (require 'tex-site) AUCTeX-version)))

(use-package latex
  :ensure nil
  :mode ("\\.tex\\'" . LaTeX-mode)
  :after auctex
  :config
  (setq TeX-engine 'xetex ;; Use XeTeX
		latex-run-command "xetex")
  (setq TeX-parse-self t ; parse on load
		TeX-auto-save t  ; parse on save
		TeX-source-correlate-mode t
		TeX-source-correlate-method 'synctex
		TeX-show-compilation nil
		;; Don't start the Emacs server when correlating sources.
		TeX-source-correlate-start-server nil
		;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
		TeX-electric-sub-and-superscript t
		;; Just save, don't ask before each compilation.
		TeX-save-query nil
		TeX-electric-sub-and-superscript t
		TeX-electric-math '("$" . "$")
		)
  (add-to-list 'TeX-view-program-list
			   `("Sioyek"
				 ("sioyek ",
				  ;;; TODO inverse search
				  ;;; FIXME, reuse-instance blocks break synctex
				  ;;; check https://github.com/ahrm/sioyek/issues/768#issuecomment-1640564592
				  "--forward-search-file "
				  "%b "
				  "--forward-search-line "
				  "%n "
				  "%o")))

  (add-to-list 'TeX-view-program-selection '(output-pdf "Sioyek"))
  (setq-default TeX-master nil)
  (setq-default TeX-command-extra-options "--shell-escape"))

;; RefTeX - 참조문헌 & Label/Ref 관리
(use-package reftex
  :ensure nil
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setq reftex-plug-into-AUCTeX t
		reftex-cite-prompt-optional-args t
		reftex-enable-partial-scans t
		reftex-save-parse-info t
		reftex-use-multiple-selection-buffers t
		reftex-toc-split-windows-fraction 0.2))

;; LaTeX mode hooks
(add-hook 'LaTeX-mode-hook #'electric-pair-local-mode) ; 자동 괄호 닫기

;; Apheleia - 자동 포맷팅 (latexindent)
(use-package apheleia
  :ensure t
  :config
  ;; latexindent 포매터 설정
  (setf (alist-get 'latexindent apheleia-formatters)
        '("latexindent" "-"))
  ;; LaTeX 모드에 latexindent 연결
  (setf (alist-get 'latex-mode apheleia-mode-alist) 'latexindent)
  (setf (alist-get 'LaTeX-mode apheleia-mode-alist) 'latexindent)
  :hook (LaTeX-mode . apheleia-mode))

;; ;; CDLatex settings
;; (use-package cdlatex
;;   :hook (LaTeX-mode . turn-on-cdlatex)
;;   :bind (:map cdlatex-mode-map
;;               ("<tab>" . cdlatex-tab)))
