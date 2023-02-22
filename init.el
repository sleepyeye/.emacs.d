(defvar user-emacs-directory "~/.emacs.d")

;; setup built-in emacs features
(load "~/.emacs.d/bootstraps.el")
(load "~/.emacs.d/defaults.el")

;;; load core packages
(load "~/.emacs.d/evil.el")
(load "~/.emacs.d/general.el")
(load "~/.emacs.d/which-key.el")
(load "~/.emacs.d/projectile.el")
(load "~/.emacs.d/completion.el")
(load "~/.emacs.d/lsp.el")
(load "~/.emacs.d/font.el")
(load "~/.emacs.d/ui.el")
(load "~/.emacs.d/magit.el")
;; (load "~/.emacs.d/tree-sitter.el")
(load "~/.emacs.d/dired.el")
(load "~/.emacs.d/workspace.el")
(load "~/.emacs.d/expand-region.el")

;;; setup languages
(load "~/.emacs.d/ansible.el")
(load "~/.emacs.d/yaml.el")
(load "~/.emacs.d/tex.el")
(load "~/.emacs.d/org.el")
(load "~/.emacs.d/quarto.el")
(load "~/.emacs.d/cc.el")
;; (load "~/.emacs.d/python.el")


;; ;; Configure Tempel
;; (elpaca-use-package tempel
;; 	;; Require trigger prefix before template name when completing.
;; 	:custom
;; 	(tempel-trigger-prefix
;; 	 "<")

;; 	:bind (("M-." . tempel-complete) ;; Alternative tempel-expand
;; 				 ("s-." . tempel-complete)
;; 				 ("M-," . tempel-insert)
;; 				 ("s-," . tempel-insert))
	
;; 	:init
;; 	;; Setup completion at point
;; 	(defun tempel-setup-capf ()
;; 		;; Add the Tempel Capf to `completion-at-point-functions'.
;; 		;; `tempel-expand' only triggers on exact matches. Alternatively use
;; 		;; `tempel-complete' if you want to see all matches, but then you
;; 		;; should also configure `tempel-trigger-prefix', such that Tempel
;; 		;; does not trigger too often when you don't expect it. NOTE: We add
;; 		;; `tempel-expand' *before* the main programming mode Capf, such
;; 		;; that it will be tried first.
;; 		(setq-local completion-at-point-functions
;; 								(cons #'tempel-expand
;; 											completion-at-point-functions)))
	
;; 	(add-hook 'prog-mode-hook 'tempel-setup-capf)
;; 	(add-hook 'text-mode-hook 'tempel-setup-capf)

;; 	;; Optionally make the Tempel templates available to Abbrev,
;; 	;; either locally or globally. `expand-abbrev' is bound to C-x '.
;; 	(add-hook 'prog-mode-hook #'tempel-abbrev-mode)
;; 	(global-tempel-abbrev-mode)
;; 	:config
;; 	(defvar sleepy/tempel-templates
;; 		'((templates "Global template"))
;; 		"My templates.")
;; 	(add-to-list 'tempel-template-sources 'sleepy/tempel-templates)
;; 	)
