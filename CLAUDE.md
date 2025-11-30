# CLAUDE.md - AI Assistant Guide for sleepyeye's Emacs Configuration

## Overview

This is a modular Emacs configuration (~3,500 lines of Elisp) focused on:
- **Vim emulation** via Evil mode with extensive text objects
- **Modern completion stack** (Vertico, Consult, Corfu, Orderless)
- **LSP integration** via Eglot for Python, C/C++, LaTeX
- **Git workflow** via Magit with AI-assisted commit messages
- **Workspace management** via Perspective

## Repository Structure

```
~/.emacs.d/
├── early-init.el      # Pre-package init: GC tuning, PATH setup, system detection
├── bootstraps.el      # Elpaca package manager bootstrap
├── init.el            # Module loader (loads all .el files in order)
│
├── Core Modules
│   ├── builtin.el     # Core Emacs settings (backups, encoding, UX)
│   ├── general.el     # Leader key bindings (SPC prefix)
│   ├── evil.el        # Vim emulation + text objects
│   ├── edit.el        # Smartparens, iedit, multiple cursors
│   ├── completion.el  # Vertico/Consult/Corfu/Orderless
│   └── font.el        # Typography configuration
│
├── Development Tools
│   ├── projectile.el  # Project management
│   ├── eglot.el       # LSP client configuration
│   ├── magit.el       # Git interface + AI commits
│   ├── workspace.el   # Perspective workspaces
│   ├── search.el      # Ripgrep integration
│   ├── tags.el        # Ctags/Citre + imenu
│   ├── register.el    # Enhanced register system (DWIM)
│   └── xref.el        # Cross-reference configuration
│
├── Language Support
│   ├── python.el      # Python + basedpyright LSP + ruff formatter
│   ├── tex.el         # LaTeX (AUCTeX + texlab)
│   ├── elisp.el       # Emacs Lisp
│   ├── markdown.el    # Markdown editing
│   └── cc.el          # C/C++ (commented out by default)
│
├── Additional
│   ├── ai.el          # Claude Code IDE integration
│   ├── ui.el          # Theme, modeline, icons
│   ├── misc.el        # Miscellaneous (vterm, etc.)
│   ├── media.el       # Image/audio/video viewing
│   └── macos.el       # macOS-specific config
│
├── local-packages/    # Vendored custom packages
│   ├── simpc-mode.el
│   └── modern-cpp-font-lock.el
│
└── docs/              # User documentation
    ├── architecture_overview.md
    ├── keybindings.md
    ├── quick_reference.md
    └── ...
```

## Key Conventions

### Naming Conventions

- **Public functions**: `sleepy/function-name`
- **Private variables**: `sleepy--variable-name`
- **Custom types**: `sleepy-type-name`
- **Constants**: `sleepy/constant-name` (with defconst)

### Module Pattern

Every module follows this structure:
```elisp
;;; module.el --- description -*- lexical-binding: t; -*-

;; Package declarations
(use-package package-name
  :ensure t
  :hook (mode . function)
  :init (setq ...)
  :config (...))

;; Custom functions
(defun sleepy/function-name ()
  "Docstring."
  ...)

;; Integration with general.el (keybindings)
(with-eval-after-load 'general
  (when (fboundp 'sleepy/leader-def)
    (sleepy/leader-def ...)))

;;; module.el ends here
```

### Package Management

Uses **Elpaca** (modern git-based package manager):
```elisp
;; Simple MELPA package
(use-package vertico :ensure t)

;; GitHub repo
(use-package pkg :ensure (:host github :repo "user/repo"))

;; Deferred loading (recommended)
(use-package pkg :ensure t :defer t)

;; Immediate loading (only for core packages)
(use-package pkg :ensure t :demand t)
```

### Keybinding Pattern

Leader key is **SPC** (in Evil normal/visual/motion states):
```elisp
(sleepy/leader-def
  "f f" 'find-file          ; File > find file
  "b b" 'consult-buffer     ; Buffer > switch
  "s p" 'consult-ripgrep    ; Search > project
  "g g" 'magit-status       ; Git > status
  "c a" 'eglot-code-actions ; Code > actions
  "p"   'projectile-command-map)  ; Project prefix
```

### System Constants

Defined in `early-init.el`:
```elisp
IS-MAC     ; (eq system-type 'darwin)
IS-LINUX   ; (memq system-type '(gnu gnu/linux ...))
IS-WINDOWS ; (memq system-type '(cygwin windows-nt ...))
IS-BSD     ; (memq system-type '(darwin berkeley-unix ...))
```

## Development Guidelines

### When Adding New Packages

1. Add to appropriate module file (or create new one)
2. Use `use-package` with `:ensure t`
3. Prefer `:defer t` for lazy loading
4. Add keybindings via `sleepy/leader-def` if needed
5. Use `with-eval-after-load` for dependencies

### When Adding New Functions

1. Prefix with `sleepy/` for public, `sleepy--` for internal
2. Always include docstring
3. Use `interactive` for user-facing commands
4. Validate inputs with `user-error` for clear messages

### When Modifying Existing Code

1. Maintain existing naming conventions
2. Keep `lexical-binding: t` in file headers
3. Test with `M-x eval-buffer` after changes
4. Run `M-x check-parens` to verify balanced parentheses

## Key Integrations

### LSP (Eglot)

Configured servers:
- **Python**: basedpyright (type checking: standard)
- **C/C++**: clangd (background indexing)
- **LaTeX**: texlab

Key bindings (SPC c ...):
- `a` - code actions
- `r` - rename symbol
- `d` - go to definition
- `D` - find references
- `h` - show documentation

### Python Development

- **Formatter**: ruff (via apheleia)
- **Environment**: PET for venv/conda detection
- **Runner**: `C-c C-c` runs current file (uv-aware)
- **LSP**: basedpyright auto-starts

### Git (Magit)

- `SPC g g` - magit-status
- AI commit messages available via Claude integration

### Workspaces (Perspective)

- `SPC TAB TAB` - switch workspace
- `SPC TAB 1-9` - jump to workspace by number
- Auto-creates workspace per project

## Testing Changes

```elisp
;; Evaluate buffer after changes
M-x eval-buffer

;; Check for syntax errors
M-x check-parens

;; Restart Emacs to test full init
;; Or use: M-x restart-emacs (if available)

;; Update packages
M-x elpaca-update-all
```

## Common Patterns

### DWIM (Do What I Mean)

Context-aware commands that adapt behavior:
```elisp
;; Register example from register.el
(sleepy/register-add-dwim ?a)  ; Stores based on context
(sleepy/register-use-dwim ?a)  ; Retrieves based on content type
```

### Hook-Based Integration

```elisp
;; Mode-specific setup
(add-hook 'python-mode-hook #'eglot-ensure)

;; Conditional loading
(add-hook 'after-init-hook #'global-auto-revert-mode)
```

### Defensive Loading

```elisp
;; Check function exists before use
(when (fboundp 'sleepy/leader-def)
  (sleepy/leader-def ...))

;; Load after dependency
(with-eval-after-load 'evil
  ...)
```

## File Locations

| Purpose | Location |
|---------|----------|
| Backups | `~/.emacs.d/backups/` |
| Autosaves | `~/.emacs.d/autosave/` |
| Custom settings | `~/.emacs.d/custom.el` |
| Bookmarks | `~/.emacs.d/bookmarks` |
| Package cache | `~/.emacs.d/elpaca/` |

## Performance Notes

- GC disabled during startup, restored to 384MB threshold after
- Lazy loading via `:defer t` is preferred
- LSP idle time set to 0.5s to reduce server communication
- File handlers temporarily disabled during startup

## AI Integration

Claude Code IDE integration in `ai.el`:
- Backend: vterm
- Trigger: `C-c C-'` or `SPC a c`
- Uses MCP (Model Context Protocol) for Emacs tools
