# RULES - Coding Standards and Conventions

**Purpose**: Coding standards and conventions overview
**Audience**: Claude Code instances for maintaining consistency
**Details**: See ~/.claude/RULES.md for complete SuperClaude framework rules

## Naming Conventions

**Functions**:
- Public: `sleepy/function-name`
- Internal: `sleepy--function-name`

**Variables**:
- Constants: `sleepy/constant-name`
- Internal: `sleepy--variable-name`

**Files**:
- Modules: `lowercase.el` (e.g., `completion.el`, `register.el`)
- No prefixes (not `sleepy-*.el`)

**Keybindings**:
- Leader: SPC prefix in normal/visual states
- Global: M-SPC
- Prefixes: Single letter (f=file, b=buffer, g=git, p=projectile)

## File Structure

All modules follow this pattern:

```elisp
;;; module.el --- description -*- lexical-binding: t; -*-

;; Package declarations
(use-package package-name
  :ensure t
  :config ...)

;; Custom functions
(defun sleepy/function-name ()
  "Documentation."
  (interactive)
  ...)

;; Keybindings (if applicable)
(with-eval-after-load 'general
  (sleepy/leader-def
    "key" '(command :which-key "description")))

;;; module.el ends here
```

## Code Quality Standards

**Required**:
- `lexical-binding: t` in all files
- Docstrings for interactive functions
- `with-eval-after-load` for cross-module integration
- `(provide 'module-name)` at end (MISSING - add in future)

**Forbidden**:
- Circular dependencies
- Hardcoded paths (use `user-emacs-directory`)
- Global state modification without guards
- Package.el usage (use Elpaca only)

## Integration Patterns

**Leader Keybindings**:
```elisp
(with-eval-after-load 'general
  (sleepy/leader-def
    "x y" '(my-command :which-key "description")))
```

**Evil State Configuration**:
```elisp
(with-eval-after-load 'evil
  (evil-set-initial-state 'my-mode 'emacs))
```

**LSP Configuration**:
```elisp
(add-to-list 'eglot-server-programs
             '(my-mode . ("language-server" "args")))
(add-hook 'my-mode-hook #'eglot-ensure)
```

## Module Creation Checklist

When creating new modules:

1. File header with `lexical-binding: t`
2. Package declarations with `:ensure`
3. Custom functions with `sleepy/` prefix
4. Keybindings via `with-eval-after-load 'general`
5. File footer `;;; module-name.el ends here`
6. Add `(provide 'module-name)` (future requirement)
7. Add to init.el load sequence

## Performance Guidelines

**Startup Optimization**:
- Use `:defer t` for non-essential packages
- Use `:hook` for mode-specific packages
- Avoid synchronous external commands in init
- Leverage GC tuning (already in early-init.el)

**Runtime Optimization**:
- Lazy load heavy packages
- Use built-in functions when available
- Avoid global hooks when buffer-local suffices
- Profile before optimizing (no premature optimization)

## Security Guidelines

- No hardcoded credentials
- No arbitrary code execution (eval-string)
- Safe file operations (expand-file-name)
- Guard external tool usage (executable-find)
- Validate user input in interactive functions

## Documentation Standards

**Inline Comments**:
- Explain "why" not "what"
- Document non-obvious patterns
- Reference related modules

**Module Documentation**:
- Purpose at file header
- Dependencies noted
- Integration points explained

**Complete Documentation**: See docs/ directory for architecture and module references

## Git Workflow

- Descriptive commit messages
- Atomic commits (single logical change)
- Test changes before committing
- Keep CLAUDE.md updated with significant changes

For complete SuperClaude framework rules including workflow rules, task management, and professional standards, see ~/.claude/RULES.md
