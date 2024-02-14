
;; This is from @jakebox
(use-package latex ;; This is a weird one. Package is auctex but needs to be managed like this.
  :ensure nil
  :mode ("\\.tex\\'" . latex-mode)
  :config
  ;; (setq TeX-engine 'xetex ;; Use XeTeX
  ;; 		latex-run-command "xetex")
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
				  ;; "--reuse-instance "
				  "--forward-search-file "
				  "%b "
				  "--forward-search-line "
				  "%n "
				  "%o")))

  (add-to-list 'TeX-view-program-selection '(output-pdf "Sioyek"))

  (setq-default TeX-master nil)
  (setq-default TeX-command-extra-options "--shell-escape"))
