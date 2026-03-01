;;; init.el --- init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(dolist (module
         '("builtin.el"
           "general.el"
           "evil.el"
           "edit.el"
           "completion.el"
           "font.el"
           "projectile.el"
           "code-navigation.el"
           "magit.el"
           "workspace.el"
           "search.el"
           "misc.el"
           "media.el"
           "tree-sitter.el"
           "markdown.el"
           "tex.el"
           "python.el"
           "elisp.el"
           "ui.el"))
  (load (expand-file-name module user-emacs-directory) nil nil t))

(when IS-MAC
  (load (expand-file-name "macos.el" user-emacs-directory) nil nil t))

(load (expand-file-name "note.el" user-emacs-directory) nil nil t)
