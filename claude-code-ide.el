;;; claude-code-ide.el --- Claude Code IDE integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for claude-code-ide.el package
;; Provides integration with Claude Code CLI for AI-assisted coding

;;; Code:

;; Install claude-code-ide from GitHub using elpaca
(use-package claude-code-ide
  :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  :demand t
  :bind (("C-c C-'" . claude-code-ide-menu))
  :config
  ;; Enable Emacs MCP tools integration
  (claude-code-ide-emacs-tools-setup)

  ;; Configuration options
  (setq claude-code-ide-cli-path "claude"              ; Claude CLI command
        claude-code-ide-terminal-backend 'vterm        ; Use vterm for terminal
        claude-code-ide-use-side-window t              ; Use side window
        claude-code-ide-window-side 'right             ; Position on right side
        claude-code-ide-window-width 90                ; Width of side window
        claude-code-ide-use-ide-diff t                 ; Enable ediff for diffs
        claude-code-ide-diagnostics-backend 'auto      ; Auto-detect flycheck/flymake
        claude-code-ide-vterm-anti-flicker t))         ; Anti-flicker for vterm

(provide 'claude-code-ide)
;;; claude-code-ide.el ends here
