 ;; setup built-in emacs features
(load "~/.emacs.d/defaults.el")
(load "~/.emacs.d/built-in.el")

;;; load core packages
(load "~/.emacs.d/general.el")
(load "~/.emacs.d/evil.el")
(load "~/.emacs.d/edit.el")
(load "~/.emacs.d/completion.el")
(elpaca-wait)
(load "~/.emacs.d/projectile.el")
(load "~/.emacs.d/eglot.el")
(load "~/.emacs.d/font.el")
(load "~/.emacs.d/ui.el")
(load "~/.emacs.d/magit.el")
(load "~/.emacs.d/workspace.el")
(load "~/.emacs.d/search.el")
(load "~/.emacs.d/misc.el")

;;; setup languages
(load "~/.emacs.d/yaml.el")
(load "~/.emacs.d/tex.el")
(load "~/.emacs.d/quarto.el")
(load "~/.emacs.d/cc.el")
(load "~/.emacs.d/python.el")
(load "~/.emacs.d/elisp.el")
