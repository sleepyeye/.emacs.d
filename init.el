;;; init.el --- init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(defun sleepy--load-module (module)
  "Load MODULE source file from `user-emacs-directory'."
  (let ((path (expand-file-name module user-emacs-directory)))
    (unless (file-readable-p path)
      (error "Config module is not readable: %s" path))
    ;; Load source file explicitly to avoid stale .elc shadowing.
    (load path nil nil t)))

;; Setup built-in emacs features
(sleepy--load-module "builtin.el")

;;; load core packages
(sleepy--load-module "general.el")
(sleepy--load-module "evil.el")
(sleepy--load-module "edit.el")
(sleepy--load-module "completion.el")
(sleepy--load-module "font.el")
(sleepy--load-module "projectile.el")
(sleepy--load-module "code-navigation.el")
(sleepy--load-module "magit.el")
(sleepy--load-module "workspace.el")
(sleepy--load-module "search.el")
(sleepy--load-module "misc.el")
(sleepy--load-module "media.el")
(sleepy--load-module "tree-sitter.el")
(sleepy--load-module "register.el")

;;; Platform-specific configuration
(when IS-MAC
  (sleepy--load-module "macos.el"))
(sleepy--load-module "note.el")

;;; setup language
;; (sleepy--load-module "yaml.el")
(sleepy--load-module "markdown.el")
(sleepy--load-module "tex.el")
;; (sleepy--load-module "quarto.el")
;; (sleepy--load-module "cc.el")
(sleepy--load-module "python.el")
(sleepy--load-module "elisp.el")

;;; Load ui stuffs
(sleepy--load-module "ui.el")
