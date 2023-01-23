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
(load "~/.emacs.d/tree-sitter.el")
(load "~/.emacs.d/dired.el")
(load "~/.emacs.d/workspace.el")

;;; setup languages
(load "~/.emacs.d/tex.el")
;; (load "~/.emacs.d/cc.el")
;; (load "~/.emacs.d/python.el")

