# Module Reference Guide

Complete reference for all configuration modules in this Emacs setup.

## Core Infrastructure Modules

### early-init.el
**Purpose**: Pre-package initialization and performance optimization  
**Load Order**: First (before package.el)  
**Dependencies**: None  

**Key Functions**:
- GC threshold manipulation (startup optimization)
- System type detection (IS-MAC, IS-LINUX, IS-WINDOWS, IS-BSD)
- PATH configuration (platform-specific)
- UI tweaks (disable toolbar, scrollbar, menu-bar)
- Performance settings (bidi, scrolling, font caches)

**Important Variables**:
- sleepy--initial-gc-cons-threshold
- sleepy--initial-gc-cons-percentage
- sleepy--initial-file-name-handler-alist
- IS-MAC, IS-LINUX, IS-WINDOWS, IS-BSD

**Integration Points**: None (runs before other modules)

---

### bootstraps.el
**Purpose**: Elpaca package manager installation and configuration  
**Load Order**: Second (after early-init.el)  
**Dependencies**: None  

**Key Functions**:
- Elpaca installer
- use-package integration
- Error handling for package operations

**Integration Points**:
- Provides Elpaca for all use-package declarations
- Must complete before any package loading

---

### init.el
**Purpose**: Module orchestration and loading  
**Load Order**: Third (after bootstraps.el)  
**Dependencies**: early-init.el, bootstraps.el  

**Module Load Sequence**:
1. builtin.el (core Emacs settings)
2. general.el (leader keys)
3. evil.el (Vim emulation)
4. edit.el (editing enhancements)
5. completion.el (modern completion stack)
6. font.el (typography)
7. projectile.el (project management)
8. eglot.el (LSP)
9. magit.el (Git)
10. workspace.el (Perspective)
11. search.el (enhanced search)
12. misc.el (miscellaneous)
13. tree-sitter.el (parsing)
14. register.el (enhanced registers)
15. ai.el (Claude Code IDE)
16. macos.el (conditional, macOS only)
17. markdown.el (Markdown support)
18. tex.el (LaTeX support)
19. python.el (Python support)
20. elisp.el (Elisp development)
21. ui.el (visual customization)

---

### builtin.el
**Purpose**: Core Emacs configuration (built-in features)  
**Load Order**: First module (loaded by init.el)  
**Dependencies**: None  

**Configuration Areas**:
- Backups and autosave (separate directories)
- User information (name, email)
- General UX (short answers, paste handling)
- Encoding (UTF-8)
- Appearance (line numbers, truncate lines)
- Whitespace (delete trailing on save)
- External tools (rg, fd)
- Compilation settings
- Auto-revert
- Uniquify (unique buffer names)
- Dired (file manager)
- Ibuffer (buffer list)

**Important Variables**:
- sleepy/backup-dir
- sleepy/autosave-dir
- sleepy/tramp-autosave-dir

**Integration Points**:
- custom.el loaded via elpaca-after-init-hook
- External tools checked via executable-find

---

## UI and Interaction Modules

### general.el
**Purpose**: Leader key system and global keybindings  
**Load Order**: Early (before modules that add leader bindings)  
**Dependencies**: None  
**Dependents**: Most modules use sleepy/leader-def  

**Key Functions**:
- rename-visited-file

**Important Variables**:
- sleepy/leader-key: "SPC"
- sleepy/global-leader-key: "M-SPC"

**Definer**:
- sleepy/leader-def (used by other modules)

**Leader Key Bindings**:
```
SPC SPC: find-file
SPC -: dired-jump
SPC :: eval-expression
SPC !: shell-command
SPC &: async-shell-command
SPC f: file operations
SPC b: buffer operations
SPC w: window management
SPC h: help/describe commands
```

**Integration Points**:
- Other modules use: (with-eval-after-load 'general ...)
- Evil integration via general-evil-setup

---

### evil.el
**Purpose**: Vim emulation and Evil mode configuration  
**Load Order**: Early (before modules that configure Evil states)  
**Dependencies**: general.el (optional, for bindings)  

**Packages**:
- evil (core Vim emulation)
- evil-collection (Evil bindings for many modes)
- evil-surround (surround text objects)
- evil-exchange (exchange text regions)
- evil-nerd-commenter (commenting operator)
- evil-textobj-line (line text object)
- evil-args (argument text objects)
- evil-indent-plus (indentation text objects)
- evil-numbers (increment/decrement numbers)
- evil-visualstar (visual selection search)
- better-jumper (jump list)

**Evil States Configuration**:
```
Emacs state: custom-mode, eshell, shell, term, vterm, 
             elpaca-ui, calc, inferior-python, wdired, log-edit
Motion state: debugger-mode, pdf-view-mode
Insert state: git-commit-mode (auto-enter)
```

**Text Objects**:
- ia/aa: arguments
- il/al: line
- if/af: function
- ig/ag: class
- ii/ai: indentation

**Operators**:
- gc: comment/uncomment
- gx/gX: exchange regions

**Navigation**:
- [b/]b: previous/next buffer
- [f/]f: previous/next function
- [g/]g: previous/next class

**Integration Points**:
- Jump list: C-o (backward), M-] (forward)
- Visual block increments: C-a/C-x with evil-numbers

---

### completion.el
**Purpose**: Modern completion stack (minibuffer + in-buffer)  
**Load Order**: Mid (after general.el)  
**Dependencies**: None  

**Packages**:
- vertico (vertical minibuffer completion)
- consult (enhanced search commands)
- orderless (flexible matching)
- marginalia (rich annotations)
- corfu (in-buffer popup completion)
- cape (completion-at-point extensions)
- consult-projectile (projectile integration)
- consult-eglot (LSP symbol search)

**Architecture**:
```
Minibuffer: Vertico UI + Consult commands + Orderless matching
In-buffer: Corfu popup + Cape sources
Annotations: Marginalia
```

**Key Functions**:
- consult-ripgrep-current
- sleepy/cape-setup (per-mode completion sources)

**Important Settings**:
- vertico-count: 12
- vertico-cycle: t
- corfu-auto: t (auto-completion)
- corfu-auto-delay: 0.1
- completion-styles: (orderless basic)

**Integration Points**:
- xref-show-xrefs-function: consult-xref
- xref-show-definitions-function: consult-xref
- register-preview-function: consult-register-format
- Cape added to completion-at-point-functions per mode

---

### edit.el
**Purpose**: Editing enhancements and text manipulation  
**Load Order**: Mid  
**Dependencies**: None  

**Packages**:
- smartparens (balanced parentheses)
- visual-regexp (visual regex replace)
- visual-regexp-steroids (enhanced regex)
- drag-stuff (move regions)
- expand-region (semantic selection expansion)
- ws-butler (whitespace cleanup)
- undo-fu (linear undo/redo)
- undo-fu-session (persistent undo)
- vundo (visual undo tree)

**Key Features**:
- Balanced parentheses editing
- Visual regex replacement
- Region manipulation
- Undo tree visualization
- Persistent undo history

---

### font.el
**Purpose**: Typography configuration  
**Load Order**: Mid  
**Dependencies**: None  

**Packages**:
- mixed-pitch (variable-pitch text modes)

**Font Configuration**:
- Default: JetBrainsMono Nerd Font
- Variable-pitch: Not configured by default
- Korean: Apple SD Gothic Neo (macOS)

**Integration Points**:
- Input method for Korean (macOS)

---

## Development Tool Modules

### projectile.el
**Purpose**: Git-root project management  
**Load Order**: Mid  
**Dependencies**: None  

**Package**: projectile

**Configuration**:
- Project type: Git repositories only
- Search paths: ~/workspace (2 levels deep)
- Indexing: Alien (external tools)
- Caching: Enabled

**Key Functions**:
- open-project-todo (create/open todo.org at project root)

**Leader Bindings**:
```
SPC p: projectile-command-map
SPC p ': open-project-todo
```

**Integration Points**:
- completion.el: consult-projectile
- workspace.el: Auto workspace per project

---

### eglot.el
**Purpose**: LSP (Language Server Protocol) client  
**Load Order**: Mid  
**Dependencies**: None  

**Package**: eglot (built-in Emacs 29+)

**Language Servers**:
```
Python: basedpyright-langserver (type checking: standard)
C/C++: clangd (background indexing, no header insertion)
LaTeX: texlab
```

**Performance**:
- read-process-output-max: 4MB (early-init.el)
- JSON-RPC logging: Disabled

**Key Settings**:
- Auto-start on mode hooks
- No inlay hints by default
- Workspace configuration per language

**Leader Bindings**:
```
SPC c s: consult-eglot-symbols
```

**Integration Points**:
- completion.el: consult-eglot
- xref: Uses consult-xref

---

### magit.el
**Purpose**: Git interface  
**Load Order**: Mid  
**Dependencies**: None  

**Packages**:
- magit (Git porcelain)
- magit-todos (TODO/FIXME in magit status)
- git-gutter (inline diff indicators)
- diff-hl (diff highlighting)

**Key Settings**:
- Display buffer: same window
- TODO keywords: TODO, FIXME, HACK, XXX, NOTE

**Leader Bindings**:
```
SPC g g: magit-status
SPC g f: magit-file-dispatch
SPC g l: magit-log-buffer-file
SPC g b: magit-blame
```

**Integration Points**:
- Evil: magit-section Evil support
- git-gutter: Visual diff indicators

---

### workspace.el
**Purpose**: Workspace isolation via Perspective  
**Load Order**: Mid  
**Dependencies**: projectile.el (functional)  

**Package**: perspective

**Configuration**:
- Auto-create workspace when opening project
- Workspace naming: Matches project name
- Buffer isolation per workspace
- Workspace persistence across sessions

**Key Functions**:
- sleepy/switch-to-workspace (with completion)

**Leader Bindings**:
```
SPC TAB TAB: persp-switch
SPC TAB [0-9]: persp-switch-by-number
SPC TAB d: persp-kill
SPC TAB r: persp-rename
```

**Integration Points**:
- projectile.el: Auto workspace per project
- completion.el: Custom consult-buffer source

---

### search.el
**Purpose**: Enhanced search commands  
**Load Order**: Mid  
**Dependencies**: completion.el (consult)  

**Key Functions**:
- sleepy/search-todos (search for TODO/FIXME/HACK/XXX/NOTE)

**Leader Bindings**:
```
SPC s s: consult-line
SPC s r: consult-ripgrep
SPC s i: consult-imenu
SPC s o: consult-outline
SPC s m: consult-mark
SPC s t: sleepy/search-todos
```

---

### tree-sitter.el
**Purpose**: Tree-sitter integration for advanced parsing  
**Load Order**: Late  
**Dependencies**: None  

**Package**: tree-sitter

**Configured Languages**:
- Python
- Emacs Lisp
- C/C++

**Integration Points**:
- Evil navigation ([f/]f for functions, [g/]g for classes)

---

### register.el
**Purpose**: Enhanced register system with file+position type  
**Load Order**: Late  
**Dependencies**: None (uses built-in registers)  

**Key Innovations**:
- sleepy-file-register struct (file + position)
- DWIM register add (context-aware)
- DWIM register use (smart jump/insert)

**Key Functions**:
- sleepy/register-save-file-position
- sleepy/register-add-dwim
- sleepy/register-use-dwim

**Leader Bindings**:
```
SPC r s: sleepy/register-save-file-position
SPC r a: sleepy/register-add-dwim
SPC r j: sleepy/register-use-dwim
SPC r i: insert-register
SPC r l: consult-register
```

**Integration Points**:
- completion.el: consult-register integration
- register-read-with-preview for completion

---

### ai.el
**Purpose**: Claude Code IDE integration  
**Load Order**: Late  
**Dependencies**: None  

**Package**: claude-code-ide

**Configuration**:
- Backend: vterm
- Anti-flicker: Enabled
- Window: Side window, right, 90 columns
- Diff: ediff integration

**Keybinding**:
- Global: C-c C-'

---

## Language Support Modules

### python.el
**Purpose**: Python development configuration  
**Load Order**: After core tools  
**Dependencies**: eglot.el (LSP)  

**Packages**:
- python (built-in)
- pip-requirements
- jupyter

**LSP Configuration**:
- Language server: basedpyright-langserver
- Type checking: standard
- Auto-start via python-mode-hook

**Key Functions**:
- sleepy/python-display-venv (show virtual environment)

**Integration Points**:
- eglot: Auto-start LSP
- jupyter: Notebook support

---

### tex.el
**Purpose**: LaTeX editing with AUCTeX  
**Load Order**: After core tools  
**Dependencies**: eglot.el (LSP)  

**Packages**:
- auctex
- cdlatex

**LSP Configuration**:
- Language server: texlab
- Auto-start via LaTeX-mode-hook

**Key Settings**:
- PDF viewer: Emacs built-in
- Auto-save: Disabled
- Parse on save: Enabled

---

### elisp.el
**Purpose**: Emacs Lisp development  
**Load Order**: After core tools  
**Dependencies**: None  

**Packages**:
- helpful (better help buffers)

**Key Functions**:
- eldoc-mode (documentation in echo area)

**Leader Bindings**:
```
SPC h f: helpful-callable
SPC h v: helpful-variable
SPC h k: helpful-key
```

---

### markdown.el
**Purpose**: Markdown editing  
**Load Order**: After core tools  
**Dependencies**: None  

**Package**: markdown-mode

---

## UI Customization Modules

### ui.el
**Purpose**: Visual customization (theme, icons, modeline)  
**Load Order**: Last (after all functionality loaded)  
**Dependencies**: None  

**Packages**:
- doom-themes
- nerd-icons
- doom-modeline
- rainbow-delimiters
- hl-todo

**Theme Configuration**:
- Default theme: doom-one
- Icon support: nerd-icons
- Modeline: doom-modeline

**Visual Enhancements**:
- Rainbow delimiters for parentheses
- TODO/FIXME highlighting
- Custom modeline configuration

---

### macos.el
**Purpose**: macOS-specific configuration  
**Load Order**: Conditional (only on macOS)  
**Dependencies**: IS-MAC constant  

**Packages**:
- exec-path-from-shell

**Configuration**:
- Frame appearance
- Input method (Korean)
- PATH synchronization from shell

---

## Module Loading Pattern

All modules follow this pattern:
```elisp
;;; module.el --- description -*- lexical-binding: t; -*-

;; Package declarations
(use-package package-name
  :ensure t
  :config ...)

;; Custom functions
(defun sleepy/function-name () ...)

;; Keybindings (if applicable)
(with-eval-after-load 'general
  (sleepy/leader-def
    "key" 'command))

;;; module.el ends here
```

## Cross-Module Integration

### Completion Integration
- consult-projectile: projectile + consult
- consult-eglot: eglot + consult
- consult-xref: xref + consult
- Custom workspace source: workspace + consult

### Evil Integration
- general-evil-setup: general + evil
- evil-collection: evil + many packages
- Tree-sitter navigation: evil + tree-sitter

### Workspace Integration
- Auto workspace creation: projectile + perspective
- Workspace buffer source: perspective + consult

This modular design allows independent development and testing of each component while maintaining clean integration points.
