# Quick Reference Guide

Fast lookup for common patterns and workflows.

## Leader Keys (SPC in normal, M-SPC global)

**Files**: SPC SPC (find-file), SPC f r (recent)  
**Buffers**: SPC b b (switch), SPC b k (kill)  
**Project**: SPC p f (find-file), SPC p p (switch project)  
**Git**: SPC g g (magit), SPC g b (blame), SPC g l (log)  
**Search**: SPC s s (line), SPC s r (ripgrep), SPC s t (todos)  
**Registers**: SPC r s (save position), SPC r j (jump), SPC r a (add)  
**Workspaces**: SPC TAB TAB (switch), SPC TAB 1-9 (by number)  
**LSP**: SPC c s (symbols), gd (definition)

## Evil Text Objects

ia/aa (args), il/al (line), if/af (function), ig/ag (class), ii/ai (indent)

## Evil Navigation

[b/]b (buffers), [f/]f (functions), [g/]g (classes), C-o (back), M-] (forward)

## Common Workflows

**Project**: SPC p p → select → auto workspace created  
**Search**: SPC s r → query → preview → select  
**Git**: SPC g g → s (stage) → c c (commit) → P p (push)  
**Register**: SPC r s (save) → work → SPC r j (jump back)  
**LSP**: gd (definition), SPC c s (symbols), M-x eglot-rename

## External Tools

rg (search), fd (find), basedpyright-langserver (Python LSP), clangd (C++ LSP), texlab (LaTeX LSP)

For detailed documentation, see architecture_overview.md and module_reference.md in this directory.
