# Emacs Configuration Architecture Overview

## Project Identity
- **Type**: Personal Emacs configuration
- **Location**: ~/.emacs.d
- **Total Files**: 27 core configuration modules
- **Lines of Code**: ~3,064 LOC
- **Package Manager**: Elpaca (modern, git-based)
- **Package Count**: 74 use-package declarations

## Architecture Philosophy

### Core Principles
1. **Simplicity over Complexity**: Prefer straightforward solutions
2. **Modern over Legacy**: No helm, ivy, or company-mode
3. **Performance over Features**: Optimized startup and runtime
4. **Modular over Monolithic**: Single responsibility per module
5. **Convention over Configuration**: Follow Emacs and package defaults

### Design Patterns
- **Layered Architecture**: 5 distinct layers with clear separation
- **Lazy Loading**: Deferred package initialization via use-package
- **Hook-based Composition**: Cross-module integration via hooks
- **Namespace Convention**: sleepy/ prefix for custom functions
- **DWIM Pattern**: Context-aware commands (Do What I Mean)

## Bootstrap Architecture

### Three-Stage Initialization

**Stage 1: early-init.el (Pre-Package)**
- Runs before package system initialization
- Performance optimizations (GC tuning, file handlers)
- System detection (IS-MAC, IS-LINUX constants)
- PATH configuration per platform
- UI tweaks (disable toolbar, scrollbar, menu-bar)
- Frame appearance setup

**Stage 2: bootstraps.el (Package Manager)**
- Elpaca package manager installation
- use-package integration
- Error handling for package operations

**Stage 3: init.el (Module Orchestration)**
- Sequential module loading via (load "path")
- Ordered dependency resolution
- Platform-specific conditional loading

### Initialization Sequence
```
early-init.el → bootstraps.el → init.el → [27 modules] → ui.el
```

## Layered Module Organization

### Layer 1: Infrastructure
- **early-init.el**: Performance tuning, PATH, system detection
- **bootstraps.el**: Elpaca package manager
- **builtin.el**: Core Emacs configuration (backups, encoding, UX)

### Layer 2: Core UI/Interaction
- **general.el**: Leader key system (SPC/M-SPC)
- **evil.el**: Vim emulation + text objects
- **completion.el**: Vertico/Consult/Corfu/Orderless stack
- **edit.el**: Editing enhancements (smartparens, visual-regexp)
- **font.el**: Typography configuration

### Layer 3: Development Tools
- **projectile.el**: Git-root project management
- **eglot.el**: LSP client (Python, C/C++, LaTeX)
- **magit.el**: Git interface + magit-todos
- **workspace.el**: Perspective integration (buffer isolation)
- **search.el**: Enhanced search (consult-ripgrep)
- **tree-sitter.el**: Advanced syntax parsing
- **register.el**: Enhanced register system (custom file+position type)
- **ai.el**: Claude Code IDE integration

### Layer 4: Language Support
- **python.el**: Python mode + Jupyter + basedpyright LSP
- **tex.el**: LaTeX (AUCTeX + texlab LSP)
- **elisp.el**: Emacs Lisp development
- **markdown.el**: Markdown editing
- **cc.el**: C/C++ (commented out by default)

### Layer 5: UI Polish
- **ui.el**: Visual customization (theme, icons, modeline)
- **macos.el**: macOS-specific tweaks (conditional loading)

## Dependency Graph

### Hard Dependencies (Load Order Critical)
```
early-init.el → bootstraps.el → all use-package
general.el → modules with leader bindings
evil.el → modules with Evil states
```

### Soft Dependencies (Functional Integration)
```
completion.el ↔ projectile.el (consult-projectile)
completion.el ↔ eglot.el (consult-eglot)
completion.el ↔ workspace.el (consult-buffer source)
workspace.el ↔ projectile.el (auto workspace per project)
```

**No Circular Dependencies**: Clean acyclic dependency graph

## Package Management Architecture

### Elpaca Pattern
- **Type**: Modern git-based package manager
- **Installation**: Parallel package fetching
- **Integration**: Seamless use-package support
- **Declaration**: 74 use-package forms across modules

### Package Declaration Patterns
1. **Simple MELPA**: `(use-package vertico :ensure t)`
2. **GitHub Direct**: `(use-package corfu :ensure (:host github :repo "minad/corfu"))`
3. **Wait Directive**: `(use-package general :ensure (:wait t))`
4. **Local Packages**: `local-packages/` directory for vendored code

### Package Loading Strategies
- **Immediate**: `:demand t` (general, consult)
- **Deferred**: `:defer t` (default for most packages)
- **Hook-based**: `:hook (mode . function)`
- **Command-based**: `:commands (func1 func2)`

## Keybinding Architecture

### Leader Key System (general.el)
- **Normal State**: SPC (space bar)
- **Global Access**: M-SPC (Meta-Space)
- **Definer**: sleepy/leader-def for consistent bindings
- **Override**: Uses override keymap to prevent conflicts

### Keybinding Hierarchy
```
Level 1: SPC f (files), SPC b (buffers), SPC g (git)
Level 2: SPC p * (projectile map), SPC TAB [0-9] (workspaces)
Level 3: SPC s s (consult-line), SPC f r (consult-recent-file)
```

### Evil Integration
- **Text Objects**: ia/aa (args), il/al (line), if/af (function), ig/ag (class)
- **Operators**: gc (comment), gx/gX (exchange)
- **Navigation**: [b/]b (buffers), [f/]f (functions), [g/]g (classes)

### Integration Pattern
```elisp
(with-eval-after-load 'general
  (sleepy/leader-def
    "key" '(command :which-key "description")))
```

## Completion Stack Architecture

### Modern Multi-Layer Stack
```
Minibuffer UI: Vertico (vertical completion)
Search Engine: Consult (enhanced commands)
In-Buffer: Corfu (popup completion)
Matching: Orderless (flexible matching)
Annotations: Marginalia (rich metadata)
Sources: Cape (completion-at-point extensions)
```

### Integration Architecture
1. **Vertico**: Replaces built-in completion UI
2. **Consult**: Overrides xref-show-xrefs-function
3. **Corfu**: Hooks into completion-at-point-functions
4. **Cape**: Adds per-mode completion sources
5. **Orderless**: Configures completion-styles

### No Legacy Packages
- No company-mode
- No ivy/counsel
- No helm

## LSP Architecture (eglot.el)

### Language Server Configuration
```
Python: basedpyright-langserver (type checking: standard)
C/C++: clangd (background indexing, no header insertion)
LaTeX: texlab
```

### Performance Tuning
- **read-process-output-max**: 4MB (set in early-init.el)
- **JSON-RPC logging**: Disabled
- **Background indexing**: Enabled for clangd

### Integration Points
- **xref**: via consult-xref (completion.el)
- **Symbol search**: consult-eglot-symbols
- **Completion**: Corfu (not company-mode)

## Workspace Management Architecture

### Perspective + Projectile Integration
```
Projectile: Git-root project detection
Perspective: Buffer isolation per workspace
Integration: Auto-create workspace when opening project
```

### Workspace Lifecycle
```
1. User opens file in git repo
2. Projectile detects project root via .git
3. Workspace auto-created/switched (matches project name)
4. Buffers isolated to workspace
5. consult-buffer shows workspace-scoped buffers first
```

### Configuration
- **Project Detection**: Git repositories only
- **Search Paths**: ~/workspace (2 levels deep)
- **Workspace Naming**: Matches project directory name
- **Buffer Sources**: Custom consult integration for workspace priority

## Register System Architecture (Innovative)

### Enhanced Register Types
```
Built-in: text, number, frameset, file
Custom: sleepy-file-register (file + position)
```

### File Register Innovation
- **Storage**: file path AND cursor position
- **Persistence**: Survives buffer closures
- **Implementation**: cl-defstruct with custom methods
- **Methods**: register-val-describe, register-val-jump-to

### DWIM Pattern
```
Context                  → Action
Active region           → Store text
Numeric prefix          → Store number
Multiple windows        → Store frameset
Default (file visiting) → Store file+position
```

### Integration
- **Preview**: register-read-with-preview
- **Completion**: Consult register functions

## Performance Optimization Architecture

### Multi-Stage Strategy

**Stage 1: Startup (early-init.el)**
```elisp
GC threshold: most-positive-fixnum (during init)
GC percentage: 0.6 (during init)
file-name-handler-alist: nil (during init)
Post-startup: 80MB threshold, 0.1 percentage
```

**Stage 2: Runtime**
```elisp
read-process-output-max: 4MB (LSP performance)
bidi-display-reordering: left-to-right only
fast-but-imprecise-scrolling: enabled
idle-update-delay: 1.0s
inhibit-compacting-font-caches: t
```

**Stage 3: Package Loading**
- Elpaca parallel installation
- use-package :defer directives
- Hook-based lazy loading
- Conditional platform loading

### Performance Characteristics
- **Startup Time**: Optimized via GC tuning
- **LSP Responsiveness**: 4MB process buffer
- **Scrolling**: Fast but imprecise mode
- **Memory**: Conservative GC post-startup

## Platform Abstraction Architecture

### System Detection
```elisp
IS-MAC: (eq system-type 'darwin)
IS-LINUX: (memq system-type '(gnu gnu/linux))
IS-WINDOWS: (memq system-type '(cygwin windows-nt))
IS-BSD: (memq system-type '(darwin berkeley-unix))
```

### Platform-Specific Configuration
```
PATH: Different for macOS vs Linux
Frame: macOS transparent titlebar
Loading: macos.el conditional on IS-MAC
Tools: executable-find checks
```

### Abstraction Quality
- Centralized in early-init.el and macos.el
- No platform assumptions scattered across modules
- Clean conditional blocks

## AI Integration Architecture

### Claude Code IDE
```
Package: claude-code-ide
Backend: vterm (terminal-based)
Anti-flicker: enabled
Window: Side window, right side, 90 columns
Diff: ediff integration
Keybinding: C-c C-'
```

### Integration Points
- Evil keybinding compatibility
- MCP (Model Context Protocol) awareness
- Emacs tool integration

## Architectural Patterns

### 1. Modular Configuration Pattern
- Each .el file = single responsibility
- No circular dependencies
- Clean separation of concerns

### 2. Lazy Loading Strategy
- use-package with :defer, :demand, :hook
- Conditional loading (IS-MAC checks)
- Deferred package initialization

### 3. Performance Optimization Pattern
- GC threshold manipulation during startup
- File handler suspension
- Native compilation support
- Batch loading via init.el

### 4. Integration Pattern
- with-eval-after-load for cross-module deps
- Definer pattern (sleepy/leader-def)
- Hook-based composition

### 5. Namespace Convention
- sleepy/ prefix for public functions
- sleepy-- prefix for internal variables
- No provide statements (architectural gap)

## Architectural Gaps

### Missing Components
1. **No provide statements**: Limits require-based loading
2. **Hardcoded paths**: Reduces portability (~/.emacs.d hardcoded)
3. **No autoload system**: Relies on explicit loads
4. **Missing dependency declarations**: No formal dependency metadata
5. **No configuration manifest**: No declarative module registry

### Improvement Opportunities
1. Add (provide 'module-name) to all modules
2. Replace hardcoded paths with user-emacs-directory
3. Implement autoload cookies
4. Add dependency headers to modules
5. Create config-manifest.el for declarative loading

## Cross-Cutting Concerns

### Documentation
- CLAUDE.md: Comprehensive guide for Claude Code
- REGISTER-GUIDE.md: Register system documentation
- Inline comments: Good coverage
- Reference guides in evil.el

### Error Handling
- bootstraps.el: condition-case-unless-debug
- Tool checks: executable-find guards
- No global error handling strategy

### Security
- No hardcoded credentials
- Safe file operations
- No arbitrary code execution
- User email in builtin.el (acceptable for personal config)

### Testing
- No test suite detected
- No CI/CD configuration
- Manual testing assumed

## Architecture Quality Assessment

### Strengths
1. Clean layered architecture (5 layers)
2. No circular dependencies
3. Modern tooling (no legacy packages)
4. Performance-conscious design
5. Sophisticated custom implementations
6. Cross-platform abstraction
7. Modular design with SRP

### Weaknesses
1. No provide statements
2. Hardcoded paths
3. No autoload system
4. Missing formal dependency declarations
5. No startup profiling

### Overall Rating: 92/100
- Architecture: 95/100
- Code Quality: 90/100
- Performance: 93/100
- Maintainability: 88/100
- Documentation: 85/100

This configuration demonstrates professional software engineering principles applied to personal Emacs configuration.
