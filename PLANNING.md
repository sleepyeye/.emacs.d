# PLANNING - Emacs Configuration System Design

**Purpose**: High-level system architecture and design decisions overview
**Audience**: Claude Code instances for understanding system design
**Details**: See docs/ directory for technical documentation

## System Architecture

5-layer modular architecture with clean dependency graph:

1. **Infrastructure**: Bootstrap, performance optimization, package management
2. **UI/Interaction**: Leader keys, Vim emulation, modern completion stack
3. **Development Tools**: LSP, Git, projects, workspaces, search
4. **Language Support**: Python, LaTeX, Elisp, Markdown
5. **UI Polish**: Themes, icons, modeline

See: docs/architecture_overview.md

## Design Principles

**Core Philosophy**:
- Simplicity over Complexity
- Modern over Legacy (no helm/ivy/company-mode)
- Performance over Features
- Modular over Monolithic
- Convention over Configuration

**Key Patterns**:
- Layered architecture (5 layers)
- Lazy loading (deferred initialization)
- Hook-based composition
- Namespace convention (sleepy/ prefix)
- DWIM (Do What I Mean) commands

**Absolute Rules** (DO NOT CHANGE):
- No circular dependencies
- Git-root-only projects (no generic directories)
- Elpaca package manager (no package.el)
- SPC leader key system
- Lexical binding in all files

## Technology Stack

**Package Manager**: Elpaca (git-based, parallel fetching)
**Completion**: Vertico + Consult + Corfu + Orderless (modern stack)
**LSP**: Eglot (built-in, performant)
**Git**: Magit + git-gutter
**Vim**: Evil + evil-collection
**Projects**: Projectile + Perspective

**Language Servers**:
- Python: basedpyright-langserver
- C/C++: clangd
- LaTeX: texlab

**Why These Choices**:
- Elpaca: Modern, declarative, parallel installation
- Vertico/Consult: Fast, modular, no legacy baggage
- Eglot: Built-in, lighter than lsp-mode
- Evil: Best Vim emulation, extensible

## Future Plans

**Next Milestones**:
- Add provide statements to all modules (portability)
- Replace hardcoded paths with user-emacs-directory
- Implement autoload system
- Add formal dependency declarations

**Refactoring Plans**:
- Create config-manifest.el for declarative loading
- Add module dependency headers
- Implement startup profiling

**Technical Debt**:
1. Missing provide statements (all modules)
2. Hardcoded ~/.emacs.d paths (38 locations)
3. No autoload system
4. No formal dependency metadata
5. No startup profiling tools

## Documentation Structure

**Root files** (this directory):
- CLAUDE.md: Guide for Claude Code instances
- PLANNING.md: This file - architecture overview
- RULES.md: Coding standards and conventions
- TASK.md: Current tasks and priorities
- KNOWLEDGE.md: Documentation index

**docs/** (detailed technical docs):
- architecture_overview.md: Complete architecture
- module_reference.md: All 27 modules
- quick_reference.md: Common patterns and workflows

## Module Dependencies

**Hard Dependencies** (load order critical):
```
early-init.el → bootstraps.el → init.el → modules → ui.el
general.el → (modules with leader bindings)
evil.el → (modules with Evil states)
```

**No Circular Dependencies**: Clean acyclic graph verified

See docs/architecture_overview.md for complete dependency analysis.
