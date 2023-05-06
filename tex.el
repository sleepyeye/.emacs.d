(use-package auctex
	:elpaca (auctex :files
			    (:defaults "*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style"))
  :defer t
  :config
  (setq-default TeX-master t))

;; This is from @jakebox
(use-feature latex ;; This is a weird one. Package is auctex but needs to be managed like this.
  :mode ("\\.tex\\'" . latex-mode)
  :init
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

  ;; To use pdfview with auctex:
  ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
  ;;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
  ;;       TeX-source-correlate-start-server t)
;; (setq-default TeX-master nil)
;; (setq-default TeX-command-extra-options "--shell-escape")
  )
