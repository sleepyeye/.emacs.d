(use-package auctex
  :ensure
  (auctex :pre-build (("./autogen.sh")
					  ("./configure"
					   "--without-texmf-dir"
					   "--with-packagelispdir=./"
					   "--with-packagedatadir=./")
					  ("make"))
		  :build (:not elpaca--compile-info) ;; Make will take care of this step
		  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
		  :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :hook ((latex-mode . LaTeX-mode))
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
  (setq-default TeX-command-extra-options "--shell-escape")
  (setq-default TeX-master t))
