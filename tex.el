(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
;; https://ogbe.net/emacs/latex
;; (setq-default TeX-master nil)
;; (setq-default TeX-command-extra-options "--shell-escape")
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq TeX-PDF-mode t)
;; (setq reftex-plug-into-AUCTeX t)
;; (setq TeX-save-query nil)
;; (setq TeX-error-overview-open-after-TeX-run t)
;; (setq TeX-electric-math '("$" . "$"))
;; (setq TeX-electric-sub-and-superscript t)

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



;; (use-package auctex
;;   :no-require t
;;   :mode ("\\.tex\\'" . LaTeX-mode)
;;   :init
;;   (setq TeX-parse-self t ; parse on load
;;         reftex-plug-into-AUCTeX t
;;         TeX-auto-save t  ; parse on save
;;         TeX-source-correlate-mode t
;;         TeX-source-correlate-method 'synctex
;;       TeX-source-correlate-start-server nil
;;       TeX-electric-sub-and-superscript t
;;       TeX-engine 'luatex ;; use lualatex by default
;;       TeX-save-query nil))

;; (use-package latex
;;   :straight auctex
;;   :general
;;   (patrl/local-leader-keys
;;     :keymaps 'LaTeX-mode-map
;;     ;; "TAB" 'TeX-complete-symbol ;; FIXME let's 'TAB' do autocompletion (but it's kind of useless to be honest)
;;     "=" '(reftex-toc :wk "reftex toc")
;;     "(" '(reftex-latex :wk "reftex label")
;;     ")" '(reftex-reference :wk "reftex ref")
;;     "m" '(LaTeX-macro :wk "insert macro")
;;     "s" '(LaTeX-section :wk "insert section header")
;;     "e" '(LaTeX-environment :wk "insert environment")
;;     "p" '(preview-at-point :wk "preview at point")
;;     "f" '(TeX-font :wk "font")
;;     "c" '(TeX-command-run-all :wk "compile"))
;;   :init
;;   (setq TeX-electric-math (cons "\\(" "\\)")) ;; '$' inserts an in-line equation '\(...\)'
;;   ;; (setq preview-scale-function 1.5) ;; too big on vivacia
;;   :config
;;   ;; (add-hook 'TeX-mode-hook #'visual-line-mode)
;;   (add-hook 'TeX-mode-hook #'reftex-mode)
;;   (add-hook 'TeX-mode-hook #'olivetti-mode)
;;   (add-hook 'TeX-mode-hook #'turn-on-auto-fill)
;;   (add-hook 'TeX-mode-hook #'prettify-symbols-mode)
;;   (add-hook 'TeX-after-compilation-finished-functions
;;               #'TeX-revert-document-buffer)
;;   (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
;;   (add-hook 'TeX-mode-hook #'outline-minor-mode)
;;   ;; (add-hook 'TeX-mode-hook #'flymake-aspell-setup)
;;   (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura")))

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


(elpaca-use-package (auctex :files
			    (:defaults "*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style"))
  :defer t
  :config
  (setq-default TeX-master t))

(elpaca-use-package auctex-latexmk
  :init 
  (with-eval-after-load 'tex
    (auctex-latexmk-setup)
    (setq-default TeX-command-default "LatexMk")
    )
  :defer t)

;; (use-package evil-tex
;;   :hook (LaTeX-mode . evil-tex-mode))

;; (add-to-list 'auto-mode-alist
;; 	     '("\\.tex\\'" . LaTeX-mode))

;; (with-eval-after-load "tex-mode"
;;   (add-hook 'tex-mode-hook #'lsp-deferred)
;;   (add-hook 'latex-mode-hook #'lsp-deferred))
;; (with-eval-after-load "bibtex"
;;   (add-hook 'bibtex-mode-hook #'lsp-deferred))
;; (setq font-latex-fontify-sectioning 'color)


(add-hook 'LaTeX-mode-hook #'lsp-deferred)

