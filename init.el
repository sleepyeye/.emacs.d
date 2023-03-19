(defvar user-emacs-directory "~/.emacs.d")
;; setup built-in emacs features
(load "~/.emacs.d/bootstraps.el")
(load "~/.emacs.d/defaults.el")


;;; load core packages
(load "~/.emacs.d/which-key.el")
(load "~/.emacs.d/evil.el")
(load "~/.emacs.d/general.el")
(load "~/.emacs.d/projectile.el")
(load "~/.emacs.d/completion.el")
(load "~/.emacs.d/lsp.el")
(load "~/.emacs.d/font.el")
(load "~/.emacs.d/ui.el")
(load "~/.emacs.d/magit.el")
;; ;;(load "~/.emacs.d/tree-sitter.el")
(load "~/.emacs.d/dired.el")
(load "~/.emacs.d/workspace.el")
(load "~/.emacs.d/expand-region.el")
;; ;; (load "~/.emacs.d/snippet.el")
(load "~/.emacs.d/search.el")
;; ;; (load "~/.emacs.d/email.el")
(load "~/.emacs.d/misc.el")

;; ;;; setup languages
;; ;;(load "~/.emacs.d/ansible.el")
;; ;;(load "~/.emacs.d/yaml.el")
(load "~/.emacs.d/tex.el")
(load "~/.emacs.d/org.el")
(load "~/.emacs.d/quarto.el")
(load "~/.emacs.d/cc.el")
(load "~/.emacs.d/python.el")

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; (use-package pdf-tools
;; 	:elpaca (pdf-tools :pre-build ("./server/autobuild") :files (:defaults "server/epdfinfo"))
;; 	:functions (pdf-isearch-batch-mode)
;; 	:commands (pdf-tools-install pdf-view-mode)
;; 	:config
;; 	(display-line-numbers-mode -1)
;; 	(add-hook 'pdf-view-mode-hook
;; 						(lambda ()
;; 							;; get rid of borders on pdf's edges
;; 							(set (make-local-variable 'evil-normal-state-cursor) (list nil))
;; 							;;for fast i-search in pdf buffers
;; 							(pdf-isearch-minor-mode)
;; 							(pdf-isearch-batch-mode)))
;; 	:mode (("\\.pdf\\'" . pdf-view-mode)))
