(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
;; https://ogbe.net/emacs/latex
(setq-default TeX-master nil)
(setq-default TeX-command-extra-options "--shell-escape")
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-save-query nil)
(setq TeX-error-overview-open-after-TeX-run t)
(setq TeX-electric-math '("$" . "$"))
(setq TeX-electric-sub-and-superscript t)

(setq TeX-parse-self t ; parse on load
      TeX-auto-save t  ; parse on save
      ;; Use hidden directories for AUCTeX files.
      TeX-auto-local ".auctex-auto"
      TeX-style-local ".auctex-style"
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex
      ;; Don't start the Emacs server when correlating sources.
      TeX-source-correlate-start-server nil
      ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
      TeX-electric-sub-and-superscript t
      ;; Just save, don't ask before each compilation.
      TeX-save-query nil)


;; Fontification taken from https://tex.stackexchange.com/a/86119/81279.
(setq font-latex-match-reference-keywords
      '(;; BibLaTeX.
	("printbibliography" "[{")
	("addbibresource" "[{")
	;; Standard commands.
	("cite" "[{")
	("citep" "[{")
	("citet" "[{")
	("Cite" "[{")
	("parencite" "[{")
	("Parencite" "[{")
	("footcite" "[{")
	("footcitetext" "[{")
	;; Style-specific commands.
	("textcite" "[{")
	("Textcite" "[{")
	("smartcite" "[{")
	("Smartcite" "[{")
	("cite*" "[{")
	("parencite*" "[{")
	("supercite" "[{")
	;; Qualified citation lists.
	("cites" "[{")
	("Cites" "[{")
	("parencites" "[{")
	("Parencites" "[{")
	("footcites" "[{")
	("footcitetexts" "[{")
	("smartcites" "[{")
	("Smartcites" "[{")
	("textcites" "[{")
	("Textcites" "[{")
	("supercites" "[{")
	;; Style-independent commands.
	("autocite" "[{")
	("Autocite" "[{")
	("autocite*" "[{")
	("Autocite*" "[{")
	("autocites" "[{")
	("Autocites" "[{")
	;; Text commands.
	("citeauthor" "[{")
	("Citeauthor" "[{")
	("citetitle" "[{")
	("citetitle*" "[{")
	("citeyear" "[{")
	("citedate" "[{")
	("citeurl" "[{")
	;; Special commands.
	("fullcite" "[{")
	;; Cleveref.
	("cref" "{")
	("Cref" "{")
	("cpageref" "{")
	("Cpageref" "{")
	("cpagerefrange" "{")
	("Cpagerefrange" "{")
	("crefrange" "{")
	("Crefrange" "{")
	("labelcref" "{")))

(setq font-latex-match-textual-keywords
      '(;; BibLaTeX brackets.
	("parentext" "{")
	("brackettext" "{")
	("hybridblockquote" "[{")
	;; Auxiliary commands.
	("textelp" "{")
	("textelp*" "{")
	("textins" "{")
	("textins*" "{")
	;; Subcaption.
	("subcaption" "[{")))

(setq font-latex-match-variable-keywords
      '(;; Amsmath.
	("numberwithin" "{")
	;; Enumitem.
	("setlist" "[{")
	("setlist*" "[{")
	("newlist" "{")
	("renewlist" "{")
	("setlistdepth" "{")
	("restartlist" "{")
	("crefname" "{")))


(use-package lsp-latex
  :defer t
  :init 
  :config
  (setq lsp-latex-forward-search-executable "/Applications/Skim.app/Contents/SharedSupport/displayline")
  (setq lsp-latex-forward-search-args '("%l" "%p" "%f")))


(use-package auctex
	:elpaca (auctex :files
			    (:defaults "*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style"))
  :defer t
  :config
  (setq-default TeX-master t))

(use-package auctex-latexmk
  :init 
  (with-eval-after-load 'tex
    (auctex-latexmk-setup)
    (setq-default TeX-command-default "LatexMk")
    )
  :defer t)


;; ;; This is from @jakebox
;; (use-package latex ;; This is a weird one. Package is auctex but needs to be managed like this.
;;   :elpacas nil
;;   :mode ("\\.tex\\'" . LaTeX-mode)
;;   :init
;;   (setq TeX-engine 'xetex ;; Use XeTeX
;;         latex-run-command "xetex")
;;   (setq TeX-parse-self t ; parse on load
;;         TeX-auto-save t  ; parse on save
;;         TeX-source-correlate-mode t
;;         TeX-source-correlate-method 'synctex
;;         TeX-show-compilation nil
;;         ;; Don't start the Emacs server when correlating sources.
;;         TeX-source-correlate-start-server nil
;;         ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
;;         TeX-electric-sub-and-superscript t
;;         ;; Just save, don't ask before each compilation.
;;         TeX-save-query nil)

;;   ;; To use pdfview with auctex:
;;   ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;   ;;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;   ;;       TeX-source-correlate-start-server t)

;;   )

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
;; (add-hook 'LaTeX-mode-hook #'lsp-deferred)

