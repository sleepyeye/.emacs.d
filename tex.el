;;; tex.el --- LaTeX editing configuration -*- lexical-binding: t; -*-

(use-package auctex
  :defer t
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
		TeX-auto-save nil  ; was t - causes lag on every save
		TeX-source-correlate-mode t
		TeX-source-correlate-method 'synctex
		TeX-show-compilation nil
		;; Don't start the Emacs server when correlating sources.
		TeX-source-correlate-start-server nil
		;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
		TeX-electric-sub-and-superscript t
		;; Just save, don't ask before each compilation.
		TeX-save-query nil
		;; TeX-electric-math disabled - conflicts with smartparens (edit.el)
		TeX-electric-math nil
		;; Use LatexMk as default command for C-c C-c
		TeX-command-default "LatexMk"
		;; Always use PDF mode
		TeX-PDF-mode t
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
  (setq-default TeX-command-extra-options "--shell-escape")

  ;; Fix AUCTeX rebinding completion key - restore completion-at-point
  (define-key TeX-mode-map [remap TeX-complete-symbol] #'completion-at-point)
  (define-key LaTeX-mode-map [remap TeX-complete-symbol] #'completion-at-point))

;; RefTeX - Bibliography & Label/Ref management
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
;; electric-pair-local-mode removed - conflicts with smartparens (edit.el)

;; Apheleia - Auto-formatting (latexindent)
(use-package apheleia
  :ensure t
  :config
  ;; latexindent formatter configuration
  ;; macOS: Use Homebrew version (avoid Perl dependency issues)
  ;; Other OS: Use latexindent from PATH
  (let ((latexindent-cmd
         (if (eq system-type 'darwin)
             ;; macOS: Check Homebrew paths
             (cond
              ((file-exists-p "/opt/homebrew/bin/latexindent")
               "/opt/homebrew/bin/latexindent")  ; Apple Silicon
              ((file-exists-p "/usr/local/bin/latexindent")
               "/usr/local/bin/latexindent")     ; Intel
              (t "latexindent"))                 ; fallback
           ;; Linux/Windows: Use PATH
           "latexindent")))
    (setf (alist-get 'latexindent apheleia-formatters)
          (list latexindent-cmd "-")))
  ;; Connect latexindent to LaTeX modes
  (setf (alist-get 'latex-mode apheleia-mode-alist) 'latexindent)
  (setf (alist-get 'LaTeX-mode apheleia-mode-alist) 'latexindent)
  :hook (LaTeX-mode . apheleia-mode))

;; Refresh git-gutter after apheleia async formatting completes
;; Without this, git-gutter updates before apheleia finishes, showing stale diff
(with-eval-after-load 'apheleia
  (add-hook 'apheleia-post-format-hook #'git-gutter))

;; ;; CDLatex settings
;; (use-package cdlatex
;;   :hook (LaTeX-mode . turn-on-cdlatex)
;;   :bind (:map cdlatex-mode-map
;;               ("<tab>" . cdlatex-tab)))

;;; tex.el ends here
