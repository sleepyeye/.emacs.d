;;; init.el --- init.el -*- no-byte-compile: t; lexical-binding: t; -*-

 ;; Setup built-in emacs features
(load "~/.emacs.d/builtin.el")

;;; load core packages
(load "~/.emacs.d/general.el")
(load "~/.emacs.d/evil.el")
(load "~/.emacs.d/edit.el")
(load "~/.emacs.d/completion.el")
(load "~/.emacs.d/font.el")
(load "~/.emacs.d/projectile.el")
(load "~/.emacs.d/eglot.el")
(load "~/.emacs.d/magit.el")
(load "~/.emacs.d/workspace.el")
(load "~/.emacs.d/search.el")
(load "~/.emacs.d/misc.el")
(load "~/.emacs.d/tree-sitter.el")
(load "~/.emacs.d/note.el")

;;; setup languages
;; (load "~/.emacs.d/yaml.el")
(load "~/.emacs.d/tex.el")
;; (load "~/.emacs.d/quarto.el")
(load "~/.emacs.d/cc.el")
(load "~/.emacs.d/python.el")
(load "~/.emacs.d/elisp.el")

;;; Load ui stuffs
(load "~/.emacs.d/ui.el")
