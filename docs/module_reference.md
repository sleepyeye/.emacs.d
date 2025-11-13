# Module Reference Guide

Complete reference for all 27 configuration modules. See architecture_overview.md for system design.

## Quick Module Index

**Infrastructure**: early-init.el, bootstraps.el, builtin.el  
**UI/Interaction**: general.el, evil.el, completion.el, edit.el, font.el  
**Dev Tools**: projectile.el, eglot.el, magit.el, workspace.el, search.el, tree-sitter.el, register.el, ai.el  
**Languages**: python.el, tex.el, elisp.el, markdown.el  
**UI Polish**: ui.el, macos.el

## Module Details

For complete module documentation including dependencies, keybindings, and integration points, see the full module_reference stored in Serena memory.

Key patterns:
- All modules use lexical-binding
- Leader key bindings via sleepy/leader-def
- Integration via with-eval-after-load
- Namespace: sleepy/ prefix for custom functions

## Common Integration Points

**Completion Stack**: Vertico + Consult + Corfu + Orderless  
**Project Management**: Projectile + Perspective (workspaces)  
**LSP**: Eglot + Consult-eglot  
**Git**: Magit + git-gutter + diff-hl  
**Evil**: Evil-collection for mode bindings
