;;; magit.el --- Git interface configuration -*- lexical-binding: t; -*-

;; transient
(use-package transient
  :config
  (transient-bind-q-to-quit))

;; magit
(use-package magit
  :commands (magit-status magit-log-all)
  :init
  ;; Open status buffer in fullscreen
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-diff-hide-trailing-cr-characters t
        ;; Auto-save unsaved buffers without asking (faster for rebase, etc.)
        magit-save-repository-buffers 'dontask
        ;; Diff refinement - highlight word-level changes in all hunks
        magit-diff-refine-hunk 'all
        ;; Log display settings (show age, width, time format)
        magit-log-margin '(t age magit-log-margin-width t 18))
  :config
  ;; Restore window layout but keep buffer (recommended default behavior)
  (setq-default magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; Auto-reflect file changes (default, but explicitly stated)
  (magit-auto-revert-mode 1))

;; Git commit message best practices
(use-package git-commit
  :ensure nil
  :after magit
  :config
  ;; Enforce commit message conventions
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 72
        git-commit-style-convention-checks '(non-empty-second-line
                                              overlong-summary-line)))

;; Magit-todos: Show TODO/FIXME in magit status
(use-package magit-todos
  :ensure t
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :config
  ;; Customize appearance
  (setq magit-todos-keywords-list '("TODO" "FIXME" "HACK" "XXX" "NOTE")
        magit-todos-exclude-globs '("*.map" "*.min.js" "*.lock")
        magit-todos-branch-list nil  ; Only scan current branch
        magit-todos-recursive t
        magit-todos-depth 100)
  ;; Use ripgrep if available for better performance
  (when (executable-find "rg")
    (setq magit-todos-scanner 'magit-todos--scan-with-ripgrep)))

;; git-gutter (VC-independent, lightweight)
(use-package git-gutter
  :hook (emacs-startup . global-git-gutter-mode)
  :init
  ;; Minimize symbols to avoid visual clutter
  (setq git-gutter:modified-sign " "
        git-gutter:added-sign    " "
        git-gutter:deleted-sign  " ")
  :config
  ;; Too frequent updates increase CPU usage → 0.2-0.3s recommended
  (setq git-gutter:update-interval 0.2
        git-gutter:hide-gutter nil)
  ;; Manage colors with set-face-attribute (avoid custom-*)
  (set-face-attribute 'git-gutter:modified nil :foreground "#553d00" :weight 'bold)
  (set-face-attribute 'git-gutter:added    nil :foreground "#005000" :weight 'bold)
  (set-face-attribute 'git-gutter:deleted  nil :foreground "#8f1313" :weight 'bold))

;; timemachine
(use-package git-timemachine
  :defer t
  :commands git-timemachine)

;; AI-powered commit message generation
(defvar sleepy/ai-commit-enabled nil
  "Flag to indicate if AI commit message should be generated automatically.")

(defun sleepy/generate-ai-commit-message ()
  "Generate commit message using Claude CLI based on staged changes.
Returns the generated message as a string, or nil on error."
  (let* ((diff (shell-command-to-string "git diff --cached"))
         (branch (magit-get-current-branch))
         (recent-commits (shell-command-to-string "git log -3 --pretty=format:'%s'"))
         (prompt (format "Analyze the following git diff and generate a concise commit message following these guidelines:

**Commit Message Format:**
- First line: 50 characters or less, imperative mood (e.g., 'Add feature' not 'Added feature')
- Blank line (if detailed explanation needed)
- Detailed explanation if needed (wrap at 72 characters per line)
- Focus on WHY the change was made, not just WHAT changed (the diff shows what)

**Context:**
- Current branch: %s
- Recent commits for style reference:
%s

**Git diff:**
```
%s
```

Generate only the commit message, no extra explanation." branch recent-commits diff)))
    (if (string-empty-p (string-trim diff))
        nil
      (let ((ai-message (string-trim
                         (shell-command-to-string
                          (format "claude --no-stream <<'EOF'\n%s\nEOF" prompt)))))
        (if (or (string-empty-p ai-message)
                (string-prefix-p "Error" ai-message))
            nil
          ai-message)))))

(defun sleepy/ai-commit-message ()
  "Generate commit message using Claude CLI based on staged changes."
  (interactive)
  (message "Generating commit message with AI...")
  (let ((ai-message (sleepy/generate-ai-commit-message)))
    (if ai-message
        (if (derived-mode-p 'git-commit-mode)
            (progn
              (goto-char (point-min))
              (insert ai-message)
              (message "AI commit message inserted. Review and edit as needed."))
          (progn
            (kill-new ai-message)
            (message "AI commit message copied to clipboard: %s"
                     (truncate-string-to-width ai-message 60 nil nil "..."))))
      (message "Failed to generate AI commit message"))))

;; Auto-insert AI message when git-commit buffer opens
(defun sleepy/git-commit-setup-with-ai ()
  "Insert AI-generated commit message if the AI option is enabled."
  (when sleepy/ai-commit-enabled
    (setq sleepy/ai-commit-enabled nil)  ; Reset flag
    (let ((ai-message (sleepy/generate-ai-commit-message)))
      (when ai-message
        (goto-char (point-min))
        (insert ai-message)
        (message "AI commit message inserted. Review and edit as needed.")))))

;; Add to git-commit-setup-hook
(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook #'sleepy/git-commit-setup-with-ai))

;; Add AI option to magit-commit transient menu
(with-eval-after-load 'magit-commit
  (transient-append-suffix 'magit-commit '(1 -1)
    '("=a" "Generate with AI" "--ai-message")))

;; Advice to capture --ai-message flag from transient
(defun sleepy/magit-commit-capture-ai-flag (args)
  "Capture --ai-message flag and set sleepy/ai-commit-enabled."
  (when (member "--ai-message" args)
    (setq sleepy/ai-commit-enabled t))
  ;; Remove --ai-message from args so it doesn't get passed to git
  (remove "--ai-message" args))

(with-eval-after-load 'magit-commit
  (advice-add 'magit-commit-create :filter-args
              (lambda (args)
                (sleepy/magit-commit-capture-ai-flag (car args))
                args)))

;; Keybindings
(with-eval-after-load 'general
  (sleepy/leader-def
    "gd" 'git-timemachine-toggle      ;; View current file's history
    "gD" 'magit-diff-buffer-file      ;; Diff current file in Magit UI
    "gE" 'ediff-buffers               ;; Manually diff current buffer vs timemachine buffer
    "g c" 'sleepy/ai-commit-message)) ;; Generate AI commit message

;; Add keybinding in git-commit-mode
(with-eval-after-load 'git-commit
  (define-key git-commit-mode-map (kbd "C-c C-a") 'sleepy/ai-commit-message))

;;; magit.el ends here
