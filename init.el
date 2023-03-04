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
;;(load "~/.emacs.d/tree-sitter.el")
(load "~/.emacs.d/dired.el")
(load "~/.emacs.d/workspace.el")
(load "~/.emacs.d/expand-region.el")
(load "~/.emacs.d/snippet.el")
(load "~/.emacs.d/search.el")

;;; setup languages
;;(load "~/.emacs.d/ansible.el")
;;(load "~/.emacs.d/yaml.el")
(load "~/.emacs.d/tex.el")
(load "~/.emacs.d/org.el")
(load "~/.emacs.d/quarto.el")
(load "~/.emacs.d/cc.el")
;; (load "~/.emacs.d/python.el")

(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))



(use-feature simple
  :general
  (+general-global-toggle
    "f" 'auto-fill-mode)
  :custom
  (eval-expression-debug-on-error nil)
  (fill-column 80 "Wrap at 80 columns."))

(use-feature autorevert
  :defer 2
  :custom
  (auto-revert-interval 0.01 "Instantaneously revert")
  :config
  (global-auto-revert-mode t))
