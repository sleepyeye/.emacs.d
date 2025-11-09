;;; ai.el --- AI tools integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for AI-powered development tools
;; Currently includes Claude Code IDE integration

;;; Code:

;; Install vterm - full featured terminal emulator
(use-package vterm
  :ensure t
  :config
  (with-eval-after-load 'evil
	(evil-set-initial-state 'vterm-mode 'emacs))

  ;; Optimize vterm for better performance
  (setq vterm-max-scrollback 5000
        vterm-timer-delay 0.01)
  ;; Disable line numbers in vterm buffers
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1))))

;; Install claude-code-ide from GitHub using elpaca
(use-package claude-code-ide
  :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  :demand t
  :bind (("C-c C-'" . claude-code-ide-menu)
         ("C-c a c" . claude-code-ide-menu)           ; Additional binding for quick access
         ("C-c a w" . sleepy/claude-write)            ; Writing assistant
         ("C-c a r" . sleepy/claude-rewrite-region)   ; Rewrite selected text
         ("C-c a p" . sleepy/claude-proofread-region) ; Proofread selected text
         ("C-c a e" . sleepy/claude-explain)          ; Explain concept/code
         ("C-c a s" . sleepy/claude-summarize))       ; Summarize text
  :config
  ;; Enable Emacs MCP tools integration
  (claude-code-ide-emacs-tools-setup)

  ;; Configuration options
  (setq claude-code-ide-cli-path "claude"              ; Claude CLI command
        claude-code-ide-terminal-backend 'vterm        ; Use vterm for terminal
        claude-code-ide-use-side-window t              ; Use side window (reduces flickering)
        claude-code-ide-window-side 'bottom            ; Open at the bottom (more stable than right)
        claude-code-ide-window-width 0.4               ; Window proportion (40% of frame)
        claude-code-ide-use-ide-diff t                 ; Enable ediff for diffs
        claude-code-ide-diagnostics-backend 'auto      ; Auto-detect flycheck/flymake
        claude-code-ide-prevent-reflow-glitch t        ; Prevent resize glitches (default: t)
        claude-code-ide-vterm-anti-flicker t           ; Enable vterm anti-flicker
        claude-code-ide-vterm-render-delay 0.1         ; Longer render delay to reduce flicker
        claude-code-ide-terminal-initialization-delay 0.5)  ; Longer delay for proper layout

  ;; Writing-specific helper functions
  (defun sleepy/claude-write ()
    "Start Claude for writing assistance in the current buffer."
    (interactive)
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (let ((mode (symbol-name major-mode))
          (topic (read-string "What would you like to write about? ")))
      (claude-code-ide-send-prompt
       (format "Help me write about: %s. I'm in %s mode. Please provide well-structured content."
               topic mode))))

  (defun sleepy/claude-rewrite-region (start end)
    "Ask Claude to rewrite the selected region."
    (interactive "r")
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (let ((text (buffer-substring-no-properties start end)))
      (claude-code-ide-send-prompt
       (format "Please rewrite the following text to be clearer and more concise:\n\n%s" text))))

  (defun sleepy/claude-proofread-region (start end)
    "Ask Claude to proofread and correct the selected region."
    (interactive "r")
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (let ((text (buffer-substring-no-properties start end)))
      (claude-code-ide-send-prompt
       (format "Please proofread the following text for grammar, spelling, and style issues. Provide corrections and suggestions:\n\n%s" text))))

  (defun sleepy/claude-explain ()
    "Ask Claude to explain a concept or code."
    (interactive)
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (let ((topic (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (read-string "What would you like explained? "))))
      (claude-code-ide-send-prompt
       (format "Please explain the following in detail:\n\n%s" topic))))

  (defun sleepy/claude-summarize ()
    "Ask Claude to summarize text or code."
    (interactive)
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (claude-code-ide-send-prompt
           (format "Please provide a concise summary of the following:\n\n%s" text)))
      (claude-code-ide-send-prompt "Please summarize the current buffer's content.")))

  ;; Evil mode integration for visual selections
  (with-eval-after-load 'evil
    (evil-define-key 'visual 'global
      (kbd "gr") 'sleepy/claude-rewrite-region
      (kbd "gp") 'sleepy/claude-proofread-region
      (kbd "ge") 'sleepy/claude-explain
      (kbd "gs") 'sleepy/claude-summarize))

  ;; Additional writing templates
  (defun sleepy/claude-email ()
    "Start Claude for email composition."
    (interactive)
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (let ((recipient (read-string "Recipient (optional): "))
          (subject (read-string "Subject/Topic: ")))
      (claude-code-ide-send-prompt
       (format "Help me write a professional email about: %s%s"
               subject
               (if (string-empty-p recipient) "" (format " to %s" recipient))))))

  (defun sleepy/claude-improve-writing ()
    "Ask Claude to improve writing style of the current buffer or region."
    (interactive)
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (claude-code-ide-send-prompt
       (format "Please improve the following text's clarity, flow, and style while maintaining its meaning:\n\n%s" text))))

  ;; Helper functions for writing assistance
  (defun claude-code-ide-improve-writing ()
    "Ask Claude to improve the selected text or current paragraph."
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'paragraph t))))
      (when text
        (claude-code-ide-send-prompt
         (format "Please improve the following text for clarity, grammar, and academic style:\n\n%s" text)))))

  (defun claude-code-ide-check-grammar ()
    "Ask Claude to check grammar of selected text or current paragraph."
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'paragraph t))))
      (when text
        (claude-code-ide-send-prompt
         (format "Check grammar and suggest corrections:\n\n%s" text)))))

  (defun claude-code-ide-make-concise ()
    "Ask Claude to make the selected text more concise."
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'paragraph t))))
      (when text
        (claude-code-ide-send-prompt
         (format "Make this text more concise while preserving meaning:\n\n%s" text)))))

  (defun claude-code-ide-expand-text ()
    "Ask Claude to expand on the selected text."
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'paragraph t))))
      (when text
        (claude-code-ide-send-prompt
         (format "Expand on this text with more detail and examples:\n\n%s" text)))))

  (defun claude-code-ide-review-abstract ()
    "Ask Claude to review the abstract section of a LaTeX document."
    (interactive)
    (when (derived-mode-p 'latex-mode 'LaTeX-mode)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "\\\\begin{abstract}" nil t)
          (let ((start (point))
                (end (when (re-search-forward "\\\\end{abstract}" nil t)
                       (match-beginning 0))))
            (when end
              (let ((abstract (buffer-substring-no-properties start end)))
                (claude-code-ide-send-prompt
                 (format "Review this LaTeX abstract and suggest improvements:\n\n%s" abstract)))))))))

  ;; Keybindings for writing functions
  (with-eval-after-load 'general
    (sleepy/leader-def
      "a i" '(claude-code-ide-improve-writing :which-key "improve writing")
      "a g" '(claude-code-ide-check-grammar :which-key "check grammar")
      "a c" '(claude-code-ide-make-concise :which-key "make concise")
      "a e" '(claude-code-ide-expand-text :which-key "expand text")
      "a r" '(claude-code-ide-review-abstract :which-key "review abstract"))))

(provide 'ai)
;;; ai.el ends here
