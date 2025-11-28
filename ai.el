;;; ai.el --- AI tools integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for AI-powered development tools

;;; Code:

(use-package claude-code-ide
  :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-terminal-backend 'vterm)
  (claude-code-ide-use-side-window t)
  (claude-code-ide-window-side 'bottom)
  :config
  (claude-code-ide-emacs-tools-setup)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'claude-code-ide-mode 'emacs)))

(provide 'ai)
;;; ai.el ends here
