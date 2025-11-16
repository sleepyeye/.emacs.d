# Claude Code IDE - Code Examples and Recipes

## Basic Usage Examples

### Example 1: Simple Prompt
```elisp
(claude-code-ide-send-prompt "Hello Claude! What can you help me with today?")
```

### Example 2: Multi-line Prompt
```elisp
(claude-code-ide-send-prompt "I need help with:
1. Understanding this complex function
2. Refactoring it for clarity
3. Adding tests

Can you help?")
```

### Example 3: Start and Send in Sequence
```elisp
(defun start-and-ask ()
  (interactive)
  (claude-code-ide)
  (sleep-for 1)  ; Wait for startup
  (claude-code-ide-send-prompt "Ready to help with coding!"))
```

---

## Advanced Usage Patterns

### Pattern 1: Analyze Current File
```elisp
(defun claude-analyze-file ()
  "Have Claude analyze the current file."
  (interactive)
  (let* ((file (buffer-file-name))
         (name (file-name-nondirectory file)))
    (claude-code-ide)
    (sleep-for 1)
    (claude-code-ide-send-prompt
     (format "Please analyze this %s file and explain what it does:\n%s"
             (file-name-extension name) name))))
```

### Pattern 2: Refactor Selection
```elisp
(defun claude-refactor-selection ()
  "Send selected code to Claude for refactoring suggestions."
  (interactive)
  (if (region-active-p)
      (let ((selected-code (buffer-substring-no-properties
                            (region-beginning) (region-end))))
        (claude-code-ide)
        (sleep-for 1)
        (claude-code-ide-send-prompt
         (format "Please refactor this code for better performance and readability:\n\n%s"
                 selected-code)))
    (user-error "No text selected")))
```

### Pattern 3: Code Review
```elisp
(defun claude-code-review (filename)
  "Request a code review of FILENAME from Claude."
  (interactive "fFile to review: ")
  (let ((code (with-temp-buffer
                (insert-file-contents filename)
                (buffer-string))))
    (claude-code-ide)
    (sleep-for 1)
    (claude-code-ide-send-prompt
     (format "Please review this code and provide feedback:\n\n%s" code))))
```

### Pattern 4: Fix Compilation Error
```elisp
(defun claude-fix-error (error-message)
  "Ask Claude to help fix a compilation error."
  (interactive "sError message: ")
  (claude-code-ide)
  (sleep-for 1)
  (claude-code-ide-send-prompt
   (format "I got this error. Can you help me fix it?\n\n%s" error-message)))
```

### Pattern 5: Continue Conversation
```elisp
(defun claude-continue-work ()
  "Continue the recent Claude conversation."
  (interactive)
  (claude-code-ide-continue)
  (sleep-for 2)
  (let ((question (read-string "Continue conversation with: ")))
    (claude-code-ide-send-prompt question)))
```

### Pattern 6: Resume and Ask
```elisp
(defun claude-resume-work ()
  "Resume a previous Claude session and ask a follow-up question."
  (interactive)
  (claude-code-ide-resume)
  (sleep-for 2)
  (claude-code-ide-send-prompt
   "What was the last thing we were working on?"))
```

### Pattern 7: Documentation Generator
```elisp
(defun claude-document-function ()
  "Generate documentation for the function at point."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((func-code (buffer-substring-no-properties
                      (point)
                      (progn (end-of-defun) (point)))))
      (claude-code-ide)
      (sleep-for 1)
      (claude-code-ide-send-prompt
       (format "Generate comprehensive documentation for this Elisp function:\n\n%s"
               func-code)))))
```

### Pattern 8: Test Generator
```elisp
(defun claude-generate-tests ()
  "Ask Claude to generate tests for the current buffer."
  (interactive)
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (claude-code-ide)
    (sleep-for 1)
    (claude-code-ide-send-prompt
     (format "Generate comprehensive unit tests for this code:\n\n%s" code))))
```

---

## Integration with Emacs Features

### Integration with Flycheck Errors
```elisp
(defun claude-fix-flycheck-errors ()
  "Ask Claude to help fix current flycheck errors."
  (interactive)
  (if (not (bound-and-true-p flycheck-mode))
      (user-error "Flycheck is not enabled")
    (let ((errors (mapcar 'flycheck-error-message
                          (flycheck-current-errors))))
      (if errors
          (progn
            (claude-code-ide)
            (sleep-for 1)
            (claude-code-ide-send-prompt
             (format "I have these linting errors. Can you help fix them?\n\n%s"
                     (mapconcat 'identity errors "\n"))))
        (message "No flycheck errors found")))))
```

### Integration with LSP
```elisp
(defun claude-explain-lsp-error ()
  "Ask Claude to explain the LSP error at point."
  (interactive)
  (if-let ((hover (lsp-describe-thing-at-point)))
      (let ((error-info (buffer-substring-no-properties
                         (point) (point-at-eol))))
        (claude-code-ide)
        (sleep-for 1)
        (claude-code-ide-send-prompt
         (format "Can you explain this LSP error?\n\n%s" error-info)))
    (user-error "No LSP information at point")))
```

### Integration with Project
```elisp
(defun claude-explain-project-structure ()
  "Ask Claude to explain the current project structure."
  (interactive)
  (if-let ((project (project-current)))
      (let ((root (project-root project)))
        (claude-code-ide)
        (sleep-for 1)
        (claude-code-ide-send-prompt
         (format "Explain the structure of this project:\n\nProject root: %s" root)))
    (user-error "Not in a project")))
```

---

## Transient Menu Integration

### Create Custom Transient Menu
```elisp
(transient-define-prefix my-claude-menu ()
  "My custom Claude Code menu."
  ["Claude Code"
   ["Prompts"
    ("a" "Analyze file" claude-analyze-file)
    ("r" "Refactor selection" claude-refactor-selection)
    ("d" "Document function" claude-document-function)
    ("t" "Generate tests" claude-generate-tests)]
   ["Session"
    ("s" "Start new" claude-code-ide)
    ("c" "Continue" claude-code-ide-continue)
    ("R" "Resume" claude-code-ide-resume)
    ("S" "Stop" claude-code-ide-stop)]
   ["Navigation"
    ("b" "Switch to buffer" claude-code-ide-switch-to-buffer)
    ("w" "Toggle window" claude-code-ide-toggle)]])

;; Bind it
(global-set-key (kbd "C-c m") 'my-claude-menu)
```

---

## Error Handling Patterns

### Pattern: Safe Prompt Sending
```elisp
(defun safe-claude-send (prompt)
  "Send a prompt to Claude with error handling."
  (condition-case err
      (if-let ((proc (claude-code-ide--get-process)))
          (if (process-live-p proc)
              (claude-code-ide-send-prompt prompt)
            (progn
              (message "Claude session died, starting new one...")
              (claude-code-ide)
              (sleep-for 1)
              (claude-code-ide-send-prompt prompt)))
        (progn
          (message "No Claude session, starting one...")
          (claude-code-ide)
          (sleep-for 1)
          (claude-code-ide-send-prompt prompt)))
    (error
     (message "Error sending prompt to Claude: %s" (error-message-string err)))))
```

### Pattern: Check Before Sending
```elisp
(defun claude-send-if-available (prompt)
  "Send prompt only if Claude is available."
  (interactive "sPrompt: ")
  (if (claude-code-ide--ensure-cli)
      (safe-claude-send prompt)
    (message "Claude Code CLI not available")))
```

---

## Batch Processing Examples

### Analyze Multiple Files
```elisp
(defun claude-analyze-project (file-pattern)
  "Analyze all matching files in the project."
  (interactive "sFile pattern (e.g., *.py): ")
  (let ((files (directory-files
                (project-root (project-current))
                t
                (wildcard-to-regexp file-pattern))))
    (claude-code-ide)
    (sleep-for 1)
    (claude-code-ide-send-prompt
     (format "Please analyze these %d files:\n%s"
             (length files)
             (mapconcat 'file-name-nondirectory files "\n")))))
```

### Refactor All Python Files
```elisp
(defun claude-refactor-python-project ()
  "Ask Claude to refactor all Python files in project."
  (interactive)
  (let ((python-files (directory-files
                       (project-root (project-current))
                       t
                       "\\.py$")))
    (claude-code-ide)
    (sleep-for 1)
    (claude-code-ide-send-prompt
     (format "Please suggest refactoring improvements for these Python files:\n%s"
             (mapconcat 'identity python-files "\n")))))
```

---

## Interactive Workflows

### Workflow: Question Loop
```elisp
(defun claude-qa-loop ()
  "Start Claude and loop asking questions."
  (interactive)
  (claude-code-ide)
  (sleep-for 1)
  (cl-loop
   for question = (read-string "Ask Claude (empty to stop): ")
   unless (string-empty-p question)
   do (claude-code-ide-send-prompt question)
   and do (sleep-for 0.5)))
```

### Workflow: Code Review Session
```elisp
(defun claude-code-review-session ()
  "Start a code review session with Claude."
  (interactive)
  (claude-code-ide)
  (sleep-for 1)
  (claude-code-ide-send-prompt
   "I'd like to do a code review. I'll send you code snippets and you provide feedback.")
  (sleep-for 2)
  (let ((code (read-string "Enter code snippet (or empty to finish): ")))
    (while (not (string-empty-p code))
      (claude-code-ide-send-prompt
       (format "Review this:\n\n%s" code))
      (setq code (read-string "Next snippet (or empty to finish): ")))))
```

---

## Helper Functions

### Check Session Status
```elisp
(defun claude-session-status ()
  "Display status of Claude sessions."
  (interactive)
  (let ((proc (claude-code-ide--get-process)))
    (if proc
        (if (process-live-p proc)
            (message "Claude is running for project: %s"
                     (claude-code-ide--get-working-directory))
          (message "Claude process is dead"))
      (message "No Claude session for current project"))))
```

### Switch and Send
```elisp
(defun claude-switch-and-send (prompt)
  "Switch to Claude buffer and send a prompt."
  (interactive "sPrompt: ")
  (claude-code-ide-switch-to-buffer)
  (sleep-for 0.2)
  (claude-code-ide-send-prompt prompt))
```

### Quick Ask
```elisp
(defun claude-quick-ask ()
  "Quick way to ask Claude something."
  (interactive)
  (let ((question (read-string "Quick question for Claude: ")))
    (if (claude-code-ide--get-process)
        (claude-code-ide-send-prompt question)
      (progn
        (claude-code-ide)
        (sleep-for 1)
        (claude-code-ide-send-prompt question)))))
```

---

## Configuration Examples

### Full Setup in Emacs Config
```elisp
;; Load claude-code-ide
(use-package claude-code-ide
  :ensure t
  :bind (("C-c c" . claude-code-ide)
         ("C-c C-c" . claude-code-ide-send-prompt)
         ("C-c C-r" . claude-code-ide-continue))
  :custom
  (claude-code-ide-window-side 'right)
  (claude-code-ide-window-width 100)
  (claude-code-ide-focus-on-open t)
  (claude-code-ide-system-prompt "You are a coding expert. Be concise but thorough.")
  (claude-code-ide-terminal-backend 'vterm)
  :config
  ;; Define helper commands
  (defun my-claude-refactor ()
    (interactive)
    (claude-refactor-selection))
  
  (defun my-claude-analyze ()
    (interactive)
    (claude-analyze-file)))
```

### Bind to Easy Keys
```elisp
;; Start Claude
(global-set-key (kbd "C-c c") 'claude-code-ide)

;; Send prompt
(global-set-key (kbd "C-c p") 'claude-code-ide-send-prompt)

;; Continue conversation
(global-set-key (kbd "C-c C-c") 'claude-code-ide-continue)

;; Toggle window
(global-set-key (kbd "C-c w") 'claude-code-ide-toggle)

;; Custom commands
(global-set-key (kbd "C-c a") 'claude-analyze-file)
(global-set-key (kbd "C-c r") 'claude-refactor-selection)
```

---

## Tips and Tricks

### Tip 1: Use with Hydra
```elisp
(defhydra hydra-claude (:color blue :hint nil)
  "Claude Code IDE"
  ("s" claude-code-ide "Start")
  ("p" claude-code-ide-send-prompt "Prompt")
  ("c" claude-code-ide-continue "Continue")
  ("r" claude-code-ide-resume "Resume")
  ("w" claude-code-ide-toggle "Toggle window")
  ("b" claude-code-ide-switch-to-buffer "Switch buffer")
  ("q" nil "Quit"))

(global-set-key (kbd "C-c h c") 'hydra-claude/body)
```

### Tip 2: Auto-start with Project
```elisp
(defun auto-start-claude ()
  (interactive)
  (when (project-current)
    (unless (claude-code-ide--get-process)
      (claude-code-ide))))

(add-hook 'find-file-hook 'auto-start-claude)
```

### Tip 3: Logging Custom Prompts
```elisp
(defvar claude-prompt-history nil)

(defun logged-claude-send (prompt)
  "Send prompt and log it."
  (push (list (current-time-string) prompt) claude-prompt-history)
  (claude-code-ide-send-prompt prompt))
```

---

## Complete Example: Documentation Generator

```elisp
(defun claude-generate-docs ()
  "Generate documentation for current buffer and display in new buffer."
  (interactive)
  (let* ((code (buffer-substring-no-properties (point-min) (point-max)))
         (filename (file-name-nondirectory (buffer-file-name)))
         (temp-buffer (get-buffer-create "*Claude Docs*")))
    ;; Start Claude
    (claude-code-ide)
    (sleep-for 1)
    
    ;; Send request
    (claude-code-ide-send-prompt
     (format "Generate comprehensive documentation for %s:\n\n%s" filename code))
    
    ;; Show temp buffer to user
    (display-buffer temp-buffer)
    (message "Claude is generating documentation. Check *Claude Docs* buffer.")))
```

---

## Summary

The most powerful and commonly used pattern:

```elisp
(defun my-claude (prompt)
  "Send a prompt to Claude, starting session if needed."
  (unless (claude-code-ide--get-process)
    (claude-code-ide)
    (sleep-for 1))
  (claude-code-ide-send-prompt prompt))

;; Use it like:
(my-claude "Explain what this function does")
(my-claude "Generate unit tests")
(my-claude "Suggest optimizations")
```
