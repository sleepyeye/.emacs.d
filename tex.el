
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
      TeX-save-query nil
      TeX-shell "/opt/homebrew/bin/fish")



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


(elpaca-use-package lsp-latex
  :defer t
  :init 
  :config
  (setq lsp-latex-forward-search-executable "/Applications/Skim.app/Contents/SharedSupport/displayline")
  (setq lsp-latex-forward-search-args '("%l" "%p" "%f")))


(elpaca-use-package auctex
  :defer t
  :config
  (setq-default TeX-master t))

(elpaca-use-package auctex-latexmk
  :defer t
  :hook TeX-mode-hook
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (setq-default TeX-command-default "LatexMk")
  :config
  (auctex-latexmk-setup))


(add-hook 'LaTeX-mode-hook '(lambda()
			      (require 'auctex-latexmk)
			      (auctex-latexmk-setup)))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(with-eval-after-load "tex-mode"
  (add-hook 'tex-mode-hook 'lsp)
  (add-hook 'latex-mode-hook 'lsp))
(with-eval-after-load "bibtex" (add-hook 'bibtex-mode-hook 'lsp))
