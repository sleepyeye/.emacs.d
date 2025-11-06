# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Emacs configuration that emphasizes:
- **Evil mode** (Vim keybindings) as the primary editing paradigm
- **Elpaca** package manager for all external packages
- **Modular configuration** split across focused `.el` files
- **Modern completion stack**: Vertico + Consult + Corfu + Orderless
- **LSP via Eglot** for Python, C/C++, LaTeX
- **Workspace management** via Perspective with Projectile integration
- **Claude Code IDE** integration for AI-assisted development

## Architecture

### Bootstrap and Initialization Flow

1. `early-init.el` - Loaded first by Emacs, sets up:
   - Performance optimizations and UI tweaks
   - PATH configuration for macOS/Linux
   - Elpaca package manager bootstrap
   - System type detection constants (`IS-MAC`, `IS-LINUX`, etc.)

2. `bootstraps.el` - Contains the Elpaca installer and use-package setup

3. `init.el` - Main entry point that loads modules in order:
   - Built-in configuration (`builtin.el`)
   - Core packages: general, evil, edit, completion, font
   - Tools: projectile, eglot, magit, workspace, search
   - Language-specific: tex, python, elisp
   - UI customization (`ui.el`)

### Module Structure

Each `.el` file is self-contained and focused on a specific domain:

- **builtin.el** - Core Emacs settings (backups, autosave, encoding, dired, etc.)
- **general.el** - Leader key (`SPC`) bindings and global keymaps
- **evil.el** - Evil mode + extensions (surround, exchange, textobj, etc.)
- **completion.el** - Minibuffer (Vertico, Consult) + in-buffer (Corfu, Cape)
- **eglot.el** - LSP configuration for Python (basedpyright), C/C++ (clangd), LaTeX (texlab)
- **projectile.el** - Git-root-only project management
- **workspace.el** - Perspective integration with project-based workspace switching
- **magit.el** - Git interface
- **ai.el** - Claude Code IDE integration with Emacs MCP tools

Language-specific modules:
- **python.el** - Python mode configuration
- **elisp.el** - Emacs Lisp development
- **tex.el** - LaTeX editing with AUCTeX
- **cc.el** - C/C++ configuration (commented out by default)

### Keybinding System

The configuration uses `general.el` with Evil mode:

- **Leader key**: `SPC` in normal/visual/motion states, `M-SPC` globally
- **Defined in**: `general.el` via `sleepy/leader-def`
- **Convention**: Each module that adds leader bindings checks for `sleepy/leader-def` before use

Key leader prefixes:
- `SPC f` - File operations
- `SPC b` - Buffer operations
- `SPC w` - Window management
- `SPC s` - Search (consult-line, consult-ripgrep, etc.)
- `SPC g` - Git (magit)
- `SPC p` - Projectile command map
- `SPC TAB [0-9]` - Workspace switching
- `SPC h` - Help/describe commands

Evil operators and text objects:
- `gc` - Comment/uncomment operator
- `gx`/`gX` - Exchange regions
- Inner/outer text objects: `ia`/`aa` (args), `il`/`al` (line), `if`/`af` (function), `ig`/`ag` (class)

## Common Development Tasks

### Package Management

**Install a new package**:
```elisp
(use-package package-name
  :ensure t
  :config
  ;; configuration here
  )
```

**Install from GitHub**:
```elisp
(use-package package-name
  :ensure (:host github :repo "user/repo")
  :config
  ;; configuration here
  )
```

**Reload Elpaca**: `M-x elpaca-process-queues`

**Update all packages**: `M-x elpaca-update-all`

### Testing Changes

After editing configuration files:
1. Evaluate the buffer: `M-x eval-buffer` or restart Emacs
2. For isolated testing, use `M-x elpaca-try` to temporarily install packages

### Project Structure

- Configuration files live directly in `~/.emacs.d/`
- Local packages (custom/vendored): `~/.emacs.d/local-packages/`
- Elpaca manages packages in `~/.emacs.d/elpaca/`
- No byte compilation for main config files (`no-byte-compile: t` in headers)

### Workspace and Project Integration

Projects are automatically detected by `.git` directory (see `projectile.el`):
- Only git repositories are considered projects
- Projects are searched in `~/workspace` (2 levels deep)
- Opening a project automatically creates/switches to a workspace named after the project
- Workspaces isolate buffers and can be saved/restored across sessions

### LSP Usage

Eglot auto-starts for configured modes (Python, C/C++, LaTeX).

Key commands:
- `M-x eglot` - Manually start LSP
- `M-x eglot-shutdown` - Stop LSP for current buffer
- `SPC cs` - `consult-eglot-symbols` (jump to symbols in project)

Language servers:
- **Python**: basedpyright-langserver (configured for "standard" type checking)
- **C/C++**: clangd (with background indexing, no header insertion)
- **LaTeX**: texlab

### Adding New Leader Keybindings

When adding keybindings in a module:

```elisp
(with-eval-after-load 'general
  (sleepy/leader-def
    "x y" '(my-command :which-key "description")))
```

Always use `with-eval-after-load 'general` to ensure general is loaded first.

## File Naming Conventions

- Main config modules: `lowercase.el` (e.g., `evil.el`, `completion.el`)
- Headers include: `;;; filename --- description -*- lexical-binding: t; -*-`
- All modules should end with: `;;; filename ends here` (or equivalent provide)
- Use `lexical-binding: t` in all files
- Mark main config files with `no-byte-compile: t` if needed

## Dependencies

### External Tools

The configuration expects these tools to be available:
- `rg` (ripgrep) - Fast search, used by projectile and consult
- `fd` - Fast find alternative
- `git` - Version control
- Language servers:
  - `basedpyright-langserver` (Python)
  - `clangd` (C/C++)
  - `texlab` (LaTeX)
- `claude` - Claude CLI for AI integration

### PATH Configuration

PATH is configured in `early-init.el` for both macOS and Linux:
- macOS: Homebrew, Cargo, TeX, Miniforge, local bin
- Linux: Cargo, local bin, Miniconda, system paths

## Evil Mode Specifics

### State Configuration

Certain modes use specific Evil states:
- **Emacs state**: custom-mode, eshell, shell, term, vterm, elpaca-ui, calc, inferior-python, wdired, log-edit
- **Motion state**: debugger-mode, pdf-view-mode
- **Insert state**: git-commit-mode (auto-enter insert for commits)

### Jump List

Uses `better-jumper` to avoid C-i/TAB conflicts:
- `C-o` - Jump backward
- `M-]` - Jump forward (instead of C-i)

## Special Integrations

### Claude Code IDE

Configured in `ai.el` with:
- Emacs MCP tools enabled for buffer/file operations
- Side window on right (90 chars wide)
- vterm backend with anti-flicker
- Ediff integration for diffs
- Bound to `C-c C-'` for quick access

### Consult Integration

Multiple packages integrate with Consult:
- Projectile → `consult-projectile`
- Eglot → `consult-eglot`
- Xref → Uses `consult-xref` by default
- Perspective → Custom workspace buffer source prioritized in `consult-buffer`

## Performance Optimizations

From `early-init.el`:
- Increased `read-process-output-max` to 4MB for LSP performance
- Disabled JSON RPC event logging in Eglot
- Alien indexing + caching in Projectile
- Native compilation warnings silenced
- Fast but imprecise scrolling enabled
- Bidirectional text optimizations for LTR-only text

## When Adding New Features

1. Create a new `.el` file if the feature is substantial enough to warrant separation
2. Add appropriate `use-package` declarations with `:ensure` directives
3. Integrate with leader keys using `sleepy/leader-def` if interactive commands are added
4. Consider Evil mode integration (operators, text objects, state settings)
5. Test with `eval-buffer` before committing
6. Update `init.el` to load the new module if needed
