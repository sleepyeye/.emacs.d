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
(load "~/.emacs.d/eglot.el")
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
(load "~/.emacs.d/tags.el")
(elpaca-wait)

;; ;;; setup languages
;; ;;(load "~/.emacs.d/ansible.el")
;; ;;(load "~/.emacs.d/yaml.el")
;; (load "~/.emacs.d/tex.el")
;; (load "~/.emacs.d/org.el")
;; (load "~/.emacs.d/quarto.el")
(load "~/.emacs.d/cc.el")
(load "~/.emacs.d/python.el")
(load "~/.emacs.d/elisp.el")
(load "~/.emacs.d/dwim-shell-command.el")
;; (load "~/.emacs.d/citar.el")

