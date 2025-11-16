# Quick Reference Guide

Fast lookup for common patterns, conventions, and workflows.

## File Locations

```
~/.emacs.d/
├── early-init.el          # Pre-package initialization
├── bootstraps.el          # Elpaca package manager
├── init.el                # Module orchestration
├── builtin.el             # Core Emacs settings
├── general.el             # Leader key system (SPC)
├── evil.el                # Vim emulation
├── completion.el          # Vertico/Consult/Corfu stack
├── register.el            # Enhanced registers
├── eglot.el               # LSP configuration
├── projectile.el          # Project management
├── workspace.el           # Perspective integration
├── magit.el               # Git interface
├── ai.el                  # Claude Code IDE
├── python.el              # Python support
├── tex.el                 # LaTeX support
├── elisp.el               # Elisp development
├── ui.el                  # Visual customization
└── CLAUDE.md              # Documentation for Claude Code

Backups: ~/.emacs.d/backups/
Autosave: ~/.emacs.d/autosave/
Packages: ~/.emacs.d/elpaca/
Custom: ~/.emacs.d/custom.el
```

## Common Keybindings

### Leader Key (SPC in normal/visual, M-SPC globally)

**Files and Buffers**
```
SPC SPC     find-file
SPC f r     consult-recent-file
SPC f f     find-file
SPC b b     consult-buffer
SPC b k     kill-buffer
SPC b s     save-buffer
```

**Projectile (SPC p)**
```
SPC p f     projectile-find-file
SPC p p     projectile-switch-project
SPC p s     consult-ripgrep (in project)
SPC p '     open-project-todo
```

**Git (SPC g)**
```
SPC g g     magit-status
SPC g f     magit-file-dispatch
SPC g l     magit-log-buffer-file
SPC g b     magit-blame
```

**Search (SPC s)**
```
SPC s s     consult-line
SPC s r     consult-ripgrep
SPC s i     consult-imenu
SPC s t     search-todos (TODO/FIXME/HACK)
```

**Registers (SPC r)**
```
SPC r s     save-file-position
SPC r a     add-dwim (context-aware)
SPC r j     use-dwim (jump/insert)
SPC r l     consult-register
```

**Workspaces (SPC TAB)**
```
SPC TAB TAB   switch-workspace
SPC TAB 1-9   workspace-by-number
SPC TAB d     kill-workspace
SPC TAB r     rename-workspace
```

**Help (SPC h)**
```
SPC h f     helpful-callable
SPC h v     helpful-variable
SPC h k     helpful-key
```

**LSP (SPC c)**
```
SPC c s     consult-eglot-symbols
```

### Evil Keybindings

**Text Objects**
```
ia/aa       argument inner/outer
il/al       line inner/outer
if/af       function inner/outer
ig/ag       class inner/outer
ii/ai       indentation inner/outer
```

**Operators**
```
gc          comment/uncomment
gx/gX       exchange regions
```

**Navigation**
```
[b / ]b     previous/next buffer
[f / ]f     previous/next function
[g / ]g     previous/next class
C-o         jump backward
M-]         jump forward
```

**G Commands**
```
gf          go to file under cursor
gd          go to definition
gc          comment operator
gu          convert to lowercase
gU          convert to uppercase
```

### Global Keybindings
```
M-x         execute-extended-command
s-x         execute-extended-command (macOS)
C-c C-'     claude-code-ide (AI assistant)
C-= / C--   text-scale-increase/decrease
```

## Common Patterns

### Adding a New Package

```elisp
;; In appropriate module file (e.g., edit.el)
(use-package package-name
  :ensure t                    ; Install from MELPA
  :defer t                     ; Lazy load (optional)
  :hook (mode . function)      ; Auto-enable (optional)
  :config
  ;; Configuration here
  )
```

### Adding Leader Keybindings

```elisp
;; In appropriate module file
(with-eval-after-load 'general
  (sleepy/leader-def
    "x y" '(my-command :which-key "description")))
```

### Adding Custom Function

```elisp
;; Use sleepy/ prefix
(defun sleepy/my-function ()
  "Description."
  (interactive)
  ;; Function body
  )
```

### Configuring Evil States

```elisp
;; In evil.el or module file
(with-eval-after-load 'evil
  (evil-set-initial-state 'my-mode 'emacs))  ; or 'motion, 'normal
```

### Adding LSP Support

```elisp
;; In eglot.el
(add-to-list 'eglot-server-programs
             '(my-mode . ("language-server-executable" "args")))
(add-hook 'my-mode-hook #'eglot-ensure)
```

## Module Creation Checklist

When creating a new module:

1. **File Header**
```elisp
;;; module-name.el --- description -*- lexical-binding: t; -*-
```

2. **Package Declarations**
```elisp
(use-package package-name
  :ensure t
  :config ...)
```

3. **Custom Functions** (if any)
```elisp
(defun sleepy/function-name () ...)
```

4. **Keybindings** (if any)
```elisp
(with-eval-after-load 'general
  (sleepy/leader-def ...))
```

5. **File Footer**
```elisp
;;; module-name.el ends here
```

6. **Add to init.el**
```elisp
(load "~/.emacs.d/module-name.el")
```

## Naming Conventions

**Functions**
- Public: `sleepy/function-name`
- Internal: `sleepy--function-name`

**Variables**
- Constants: `sleepy/constant-name`
- Internal: `sleepy--variable-name`

**Files**
- Modules: `lowercase.el` (e.g., `completion.el`)
- No prefixes (not `sleepy-*.el`)

**Keybindings**
- Leader: SPC prefix
- Prefixes: Single letter (f for file, b for buffer, g for git)

## Common Workflows

### Opening a Project
1. `SPC p p` (projectile-switch-project)
2. Select project from list
3. Workspace auto-created with project name
4. Buffers isolated to workspace

### Searching in Project
1. `SPC s r` (consult-ripgrep)
2. Type search query
3. Preview results live
4. Select to jump to location

### Git Workflow
1. Make changes
2. `SPC g g` (magit-status)
3. Stage changes: `s` on files
4. Commit: `c c`, write message, `C-c C-c`
5. Push: `P p`

### Register Workflow
1. **Save location**: `SPC r s` (saves file + position)
2. **Jump back**: `SPC r j` (jumps to saved location)
3. **Save region**: Select region, `SPC r a` (stores text)
4. **Insert text**: `SPC r j` (inserts stored text)

### LSP Workflow
1. Open file (LSP auto-starts for Python/C++/LaTeX)
2. Go to definition: `gd`
3. Find references: `SPC c s` then search
4. Rename: `M-x eglot-rename`
5. Format: `M-x eglot-format`

### Claude Code IDE Workflow
1. Open AI assistant: `C-c C-'`
2. Ask questions or request code changes
3. Review diffs with ediff integration
4. Accept or reject changes

## Performance Tips

**Startup Optimization**
- GC threshold: Increased during startup, restored after
- File handlers: Suspended during startup
- Lazy loading: Most packages deferred

**Runtime Performance**
- LSP buffer: 4MB for responsiveness
- Fast scrolling: Enabled but imprecise
- Font caches: Not compacted
- Bidi: Disabled (LTR only)

## Troubleshooting

**Package Issues**
```elisp
M-x elpaca-log              ; Check package installation
M-x elpaca-rebuild-all      ; Rebuild all packages
M-x elpaca-update-all       ; Update all packages
```

**LSP Issues**
```elisp
M-x eglot-shutdown          ; Stop LSP server
M-x eglot-reconnect         ; Reconnect to server
M-x eglot                   ; Start LSP manually
```

**Keybinding Conflicts**
```elisp
M-x describe-key (C-h k)    ; Check what key does
M-x where-is                ; Find command keybinding
```

**Performance Issues**
```elisp
M-x emacs-init-time         ; Check startup time
M-x profiler-start          ; Start profiler
M-x profiler-report         ; View profile
```

## Configuration Reloading

**Reload Module**
```elisp
M-x eval-buffer             ; In module file
; or
M-x load-file               ; Load specific file
```

**Full Restart**
```
Kill Emacs, restart
; or  
M-x restart-emacs (if installed)
```

## External Dependencies

**Required Tools**
- rg (ripgrep): Fast search
- fd: Fast find
- git: Version control

**Language Servers**
- basedpyright-langserver (Python)
- clangd (C/C++)
- texlab (LaTeX)

**Optional Tools**
- claude: Claude CLI for AI integration

## Integration Points

**Completion Stack**
```
Vertico → UI
Consult → Commands
Corfu → Popup
Orderless → Matching
Marginalia → Annotations
Cape → Sources
```

**Project + Workspace**
```
Projectile → Project detection (git root)
Perspective → Workspace isolation
Integration → Auto workspace per project
```

**LSP + Completion**
```
Eglot → LSP client
Consult-eglot → Symbol search
Corfu → Completion UI
```

**Git**
```
Magit → Git interface
Magit-todos → TODO tracking
Git-gutter → Inline diffs
```

This quick reference provides fast access to common patterns and workflows in this Emacs configuration.
