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
    "Start Claude for writing assistance in the current buffer.

This function validates that Claude Code IDE is available and starts
a session if needed. It prompts for a topic and provides context about
the current major mode."
    (interactive)
    (unless (fboundp 'claude-code-ide-session-active-p)
      (user-error "Claude Code IDE is not available"))
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (let ((mode (symbol-name major-mode))
          (topic (read-string "What would you like to write about? ")))
      (when (string-empty-p topic)
        (user-error "Topic cannot be empty"))
      (claude-code-ide-send-prompt
       (format "Help me write about: %s. I'm in %s mode. Please provide well-structured content."
               topic mode))))

  (defun sleepy/claude-rewrite-region (start end)
    "Ask Claude to rewrite the text between START and END.

The region must be active and non-empty. Claude Code IDE must be available."
    (interactive "r")
    (unless (fboundp 'claude-code-ide-session-active-p)
      (user-error "Claude Code IDE is not available"))
    (unless (use-region-p)
      (user-error "No active region"))
    (when (= start end)
      (user-error "Region is empty"))
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (let ((text (buffer-substring-no-properties start end)))
      (claude-code-ide-send-prompt
       (format "Please rewrite the following text to be clearer and more concise:\n\n%s" text))))

  (defun sleepy/claude-proofread-region (start end)
    "Ask Claude to proofread and correct the text between START and END.

The region must be active and non-empty. Claude Code IDE must be available."
    (interactive "r")
    (unless (fboundp 'claude-code-ide-session-active-p)
      (user-error "Claude Code IDE is not available"))
    (unless (use-region-p)
      (user-error "No active region"))
    (when (= start end)
      (user-error "Region is empty"))
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (let ((text (buffer-substring-no-properties start end)))
      (claude-code-ide-send-prompt
       (format "Please proofread the following text for grammar, spelling, and style issues. Provide corrections and suggestions:\n\n%s" text))))

  (defun sleepy/claude-explain ()
    "Ask Claude to explain a concept or code.

If a region is active, explains the selected text.
Otherwise, prompts for input. Claude Code IDE must be available."
    (interactive)
    (unless (fboundp 'claude-code-ide-session-active-p)
      (user-error "Claude Code IDE is not available"))
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (let ((topic (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (read-string "What would you like explained? "))))
      (when (string-empty-p topic)
        (user-error "Nothing to explain"))
      (claude-code-ide-send-prompt
       (format "Please explain the following in detail:\n\n%s" topic))))

  (defun sleepy/claude-summarize ()
    "Ask Claude to summarize text or code.

If a region is active, summarizes the selected text.
Otherwise, summarizes the entire buffer. Claude Code IDE must be available."
    (interactive)
    (unless (fboundp 'claude-code-ide-session-active-p)
      (user-error "Claude Code IDE is not available"))
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (when (string-empty-p text)
            (user-error "Region is empty"))
          (claude-code-ide-send-prompt
           (format "Please provide a concise summary of the following:\n\n%s" text)))
      (claude-code-ide-send-prompt "Please summarize the current buffer's content.")))

  ;; Additional writing templates
  (defun sleepy/claude-email ()
    "Start Claude for email composition.

Prompts for subject and optional recipient. Claude Code IDE must be available."
    (interactive)
    (unless (fboundp 'claude-code-ide-session-active-p)
      (user-error "Claude Code IDE is not available"))
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (let ((recipient (read-string "Recipient (optional): "))
          (subject (read-string "Subject/Topic: ")))
      (when (string-empty-p subject)
        (user-error "Subject cannot be empty"))
      (claude-code-ide-send-prompt
       (format "Help me write a professional email about: %s%s"
               subject
               (if (string-empty-p recipient) "" (format " to %s" recipient))))))

  (defun sleepy/claude-improve-writing ()
    "Ask Claude to improve writing style of the current buffer or region.

If a region is active, improves only the selected text.
Otherwise, improves the entire buffer. Claude Code IDE must be available."
    (interactive)
    (unless (fboundp 'claude-code-ide-session-active-p)
      (user-error "Claude Code IDE is not available"))
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (when (string-empty-p text)
        (user-error "No text to improve"))
      (claude-code-ide-send-prompt
       (format "Please improve the following text's clarity, flow, and style while maintaining its meaning:\n\n%s" text))))

  ;; Helper functions for writing assistance
  (defun sleepy/claude-improve-paragraph ()
    "Ask Claude to improve the selected text or current paragraph.

If a region is active, improves the selected text.
Otherwise, improves the current paragraph. Claude Code IDE must be available."
    (interactive)
    (unless (fboundp 'claude-code-ide-session-active-p)
      (user-error "Claude Code IDE is not available"))
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'paragraph t))))
      (unless text
        (user-error "No text found to improve"))
      (when (string-empty-p (string-trim text))
        (user-error "Text is empty"))
      (unless (claude-code-ide-session-active-p)
        (claude-code-ide))
      (claude-code-ide-send-prompt
       (format "Please improve the following text for clarity, grammar, and academic style:\n\n%s" text))))

  (defun sleepy/claude-check-grammar ()
    "Ask Claude to check grammar of selected text or current paragraph.

If a region is active, checks the selected text.
Otherwise, checks the current paragraph. Claude Code IDE must be available."
    (interactive)
    (unless (fboundp 'claude-code-ide-session-active-p)
      (user-error "Claude Code IDE is not available"))
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'paragraph t))))
      (unless text
        (user-error "No text found to check"))
      (when (string-empty-p (string-trim text))
        (user-error "Text is empty"))
      (unless (claude-code-ide-session-active-p)
        (claude-code-ide))
      (claude-code-ide-send-prompt
       (format "Check grammar and suggest corrections:\n\n%s" text))))

  (defun sleepy/claude-make-concise ()
    "Ask Claude to make the selected text more concise.

If a region is active, processes the selected text.
Otherwise, processes the current paragraph. Claude Code IDE must be available."
    (interactive)
    (unless (fboundp 'claude-code-ide-session-active-p)
      (user-error "Claude Code IDE is not available"))
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'paragraph t))))
      (unless text
        (user-error "No text found to make concise"))
      (when (string-empty-p (string-trim text))
        (user-error "Text is empty"))
      (unless (claude-code-ide-session-active-p)
        (claude-code-ide))
      (claude-code-ide-send-prompt
       (format "Make this text more concise while preserving meaning:\n\n%s" text))))

  (defun sleepy/claude-expand-text ()
    "Ask Claude to expand on the selected text.

If a region is active, expands the selected text.
Otherwise, expands the current paragraph. Claude Code IDE must be available."
    (interactive)
    (unless (fboundp 'claude-code-ide-session-active-p)
      (user-error "Claude Code IDE is not available"))
    (let ((text (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'paragraph t))))
      (unless text
        (user-error "No text found to expand"))
      (when (string-empty-p (string-trim text))
        (user-error "Text is empty"))
      (unless (claude-code-ide-session-active-p)
        (claude-code-ide))
      (claude-code-ide-send-prompt
       (format "Expand on this text with more detail and examples:\n\n%s" text))))

  (defun sleepy/claude-review-abstract ()
    "Ask Claude to review the abstract section of a LaTeX document.

Only works in LaTeX-mode or latex-mode. Searches for \\begin{abstract}...\\end{abstract}
and sends that content to Claude for review. Claude Code IDE must be available."
    (interactive)
    (unless (fboundp 'claude-code-ide-session-active-p)
      (user-error "Claude Code IDE is not available"))
    (unless (derived-mode-p 'latex-mode 'LaTeX-mode)
      (user-error "Not in a LaTeX buffer"))
    (unless (claude-code-ide-session-active-p)
      (claude-code-ide))
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "\\\\begin{abstract}" nil t)
        (user-error "No abstract found in document"))
      (let ((start (point))
            (end (when (re-search-forward "\\\\end{abstract}" nil t)
                   (match-beginning 0))))
        (unless end
          (user-error "Abstract begin found but no end tag"))
        (let ((abstract (buffer-substring-no-properties start end)))
          (when (string-empty-p (string-trim abstract))
            (user-error "Abstract is empty"))
          (claude-code-ide-send-prompt
           (format "Review this LaTeX abstract and suggest improvements:\n\n%s" abstract)))))))

  ;; Keybindings for writing functions
  (with-eval-after-load 'general
    (when (fboundp 'sleepy/leader-def)
      (sleepy/leader-def
        "a i" '(sleepy/claude-improve-paragraph :which-key "improve paragraph")
        "a g" '(sleepy/claude-check-grammar :which-key "check grammar")
        "a c" '(sleepy/claude-make-concise :which-key "make concise")
        "a x" '(sleepy/claude-expand-text :which-key "expand text")
        "a A" '(sleepy/claude-review-abstract :which-key "review abstract"))))

  ;; Guard evil integration
  (with-eval-after-load 'evil
    (when (fboundp 'evil-define-key)
      (evil-define-key 'visual 'global
        (kbd "gr") 'sleepy/claude-rewrite-region
        (kbd "gp") 'sleepy/claude-proofread-region
        (kbd "ge") 'sleepy/claude-explain
        (kbd "gs") 'sleepy/claude-summarize))))

(provide 'ai)
;;; ai.el ends here
