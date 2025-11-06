;;; ai.el --- AI tools integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for AI-powered development tools
;; Currently includes Claude Code IDE integration

;;; Code:

;; Install eat (Emulate A Terminal) - faster alternative to vterm
(use-package eat
  :ensure (:host codeberg :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  ;; Eat-specific optimizations for better performance
  (setq eat-kill-buffer-on-exit t
        eat-enable-yank-to-terminal t))

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
        claude-code-ide-terminal-backend 'eat          ; Use eat for terminal (faster, less flickering)
        claude-code-ide-use-side-window t              ; Use side window
        claude-code-ide-window-side 'bottom            ; Open at the bottom
        claude-code-ide-window-height 30               ; Custom height (30 lines)
        claude-code-ide-use-ide-diff t                 ; Enable ediff for diffs
        claude-code-ide-diagnostics-backend 'auto)     ; Auto-detect flycheck/flymake

  ;; Eat-specific configuration for Claude Code
  (with-eval-after-load 'eat
    ;; Disable line numbers in eat buffers
    (add-hook 'eat-mode-hook (lambda () (display-line-numbers-mode -1)))
    ;; Optimize eat performance for Claude Code
    (add-hook 'eat-mode-hook
              (lambda ()
                (setq-local scroll-margin 0)
                (setq-local scroll-conservatively 0)
                (setq-local hscroll-margin 0)
                ;; Disable modes that might cause redraw issues
                (when (bound-and-true-p hl-line-mode)
                  (hl-line-mode -1))))))

(provide 'ai)
;;; ai.el ends here
