# Claude Code IDE - Complete Documentation Index

This directory now contains comprehensive documentation for the claude-code-ide Emacs package.

## Documentation Files

### 1. **claude-code-ide-API.md** (733 lines)
   **Comprehensive API Reference**
   - All 188 functions documented
   - Main interactive commands (15+)
   - Internal helper functions
   - MCP tool functions
   - Customization variables
   - Error handling
   - Performance details
   - File structure reference

   **Best for**: Understanding the complete API, looking up specific functions, learning implementation details

### 2. **claude-code-ide-QUICK-REFERENCE.md**
   **Quick Lookup Guide**
   - Essential functions for prompts/messages
   - Interactive commands at a glance
   - Key function signatures
   - Session management
   - Terminal control
   - Customization options
   - Troubleshooting
   - Common workflows

   **Best for**: Quick lookups, cheat sheet, everyday reference

### 3. **claude-code-ide-EXAMPLES.md**
   **Code Examples and Recipes**
   - Basic usage examples
   - Advanced usage patterns
   - Integration examples
   - Error handling patterns
   - Batch processing
   - Custom functions
   - Configuration examples
   - Tips and tricks

   **Best for**: Learning by example, copying code patterns, building custom features

### 4. **claude-code-ide-INDEX.md** (this file)
   **Navigation and Overview**
   - File descriptions
   - Quick links
   - Key functions summary
   - Getting started guide

   **Best for**: Navigating the documentation

---

## Quick Start

### For New Users
1. Read this index
2. Check "Getting Started" below
3. Look at QUICK-REFERENCE.md for basic commands
4. Try examples from EXAMPLES.md

### For API Developers
1. Start with API.md introduction
2. Find specific functions you need
3. Check EXAMPLES.md for usage patterns
4. Refer to source files in `/Users/sleepyeye/.emacs.d/elpaca/repos/claude-code-ide/`

### For Integration
1. Review EXAMPLES.md for integration patterns
2. Check API.md for available functions
3. Look at your CLAUDE.md for project conventions

---

## Getting Started

### Install and Enable
```elisp
;; Already installed via Elpaca
;; Add to your config:
(require 'claude-code-ide)
(claude-code-ide-emacs-tools-setup)  ; Enable Emacs MCP tools
```

### First Session
```elisp
;; 1. Start Claude
M-x claude-code-ide

;; 2. Send a prompt
M-x claude-code-ide-send-prompt
;; Type: "Hello! Can you help me with my project?"

;; 3. See Claude respond in the terminal
```

### Most Used Commands
```elisp
(claude-code-ide)                    ; Start new session
(claude-code-ide-send-prompt)        ; Send a message
(claude-code-ide-continue)           ; Continue recent work
(claude-code-ide-switch-to-buffer)   ; Focus Claude window
(claude-code-ide-stop)               ; Stop current session
```

---

## Core Functions Summary

### Session Management
| Function | Purpose |
|----------|---------|
| `claude-code-ide` | Start new session |
| `claude-code-ide-continue` | Continue recent conversation |
| `claude-code-ide-resume` | Resume previous conversation |
| `claude-code-ide-stop` | Stop current session |
| `claude-code-ide-check-status` | Verify CLI is installed |

### Prompts and Messages
| Function | Purpose |
|----------|---------|
| `claude-code-ide-send-prompt` | Send text/prompt to Claude |
| `claude-code-ide-insert-at-mentioned` | Send selected text to Claude |

### Window Management
| Function | Purpose |
|----------|---------|
| `claude-code-ide-switch-to-buffer` | Focus Claude window |
| `claude-code-ide-toggle` | Toggle window visibility |
| `claude-code-ide-toggle-recent` | Toggle most recent window |
| `claude-code-ide-list-sessions` | Switch between sessions |

### Terminal Control
| Function | Purpose |
|----------|---------|
| `claude-code-ide-send-escape` | Send escape key |
| `claude-code-ide-insert-newline` | Send newline in prompt |

---

## Key Features

### Multi-Project Support
- Each project gets its own Claude session
- Sessions identified by git root
- Switch between sessions with `claude-code-ide-list-sessions`

### MCP Integration
- Automatic Model Context Protocol server
- Claude can interact with Emacs
- Tools: file operations, diagnostics, project info, xref, imenu, tree-sitter

### Terminal Backends
- **vterm**: Default, fully-featured (requires C compilation)
- **eat**: Alternative, pure Elisp implementation
- Change via `claude-code-ide-terminal-backend`

### Customizable Display
- Side window positioning (left, right, top, bottom)
- Window sizing
- Auto-focus behavior
- Focus after ediff

### Anti-Flicker Rendering
- Smart batching for terminal updates (vterm only)
- Automatic pattern detection
- Configurable via `claude-code-ide-vterm-anti-flicker`

### System Prompt Integration
- Always includes Emacs context information
- Tells Claude about coordinate system (1-based lines, 0-based columns)
- Lists available tools
- Can be customized via `claude-code-ide-system-prompt`

---

## Important Concepts

### Working Directory
- Automatically uses project root (via project.el)
- Falls back to `default-directory` if not in project
- Critical for multi-project support

### Session ID
- Unique identifier for each session
- Format: `claude-[project-name]-[timestamp]`
- Used for MCP server identification
- Stored in `~/.claude/ide/` lockfiles

### CLI Flags
- `-c`: Continue recent conversation
- `-r`: Resume previous conversation
- `-d`: Debug mode
- `--append-system-prompt`: System prompt
- `--mcp-config`: MCP tools configuration
- `--allowedTools`: Tool restrictions

### Terminal Communication
- Text sent via `stdin` to Claude terminal
- 0.1 second delay built-in before return
- Works with both vterm and eat backends

---

## Architecture Overview

### Files Involved
```
claude-code-ide.el                  ; Main entry point, session management
claude-code-ide-mcp.el              ; WebSocket server, session tracking
claude-code-ide-mcp-handlers.el     ; MCP tool implementations
claude-code-ide-mcp-server.el       ; HTTP-based tools server
claude-code-ide-emacs-tools.el      ; Emacs-specific tools
claude-code-ide-diagnostics.el      ; Flycheck/flymake integration
claude-code-ide-transient.el        ; Transient menus
claude-code-ide-debug.el            ; Debug logging
```

### Process Flow
```
1. User calls (claude-code-ide)
2. Detects project root
3. Starts MCP server (WebSocket)
4. Creates terminal buffer (vterm or eat)
5. Runs: claude --append-system-prompt "..." --mcp-config "..."
6. Claude CLI connects to MCP server
7. Emacs and Claude communicate bidirectionally
8. When Claude exits, cleanup triggered
```

---

## Common Use Cases

### Use Case 1: Ask Questions
```elisp
(claude-code-ide)
(sleep-for 1)
(claude-code-ide-send-prompt "Explain this function to me")
```

### Use Case 2: Code Review
```elisp
;; Select code, then:
(claude-code-ide-insert-at-mentioned)
;; Claude receives the selection
(claude-code-ide-send-prompt "Review this code")
```

### Use Case 3: Continue Session
```elisp
(claude-code-ide-continue)
(sleep-for 1)
(claude-code-ide-send-prompt "What's next?")
```

### Use Case 4: Multi-Project Work
```elisp
;; Project A
(claude-code-ide)
;; ... work ...

;; Switch to Project B
(find-file "~/projects/other/main.py")
(claude-code-ide)

;; Switch between sessions
(claude-code-ide-list-sessions)
```

---

## Configuration Tips

### For Better Performance
```elisp
;; Increase process buffer
(setq read-process-output-max (* 4 1024 1024))

;; Enable anti-flicker (vterm only)
(setq claude-code-ide-vterm-anti-flicker t)

;; Set appropriate delays
(setq claude-code-ide-terminal-initialization-delay 0.1)
(setq claude-code-ide-vterm-render-delay 0.005)
```

### For Better Organization
```elisp
;; Show on right side
(setq claude-code-ide-window-side 'right)
(setq claude-code-ide-window-width 100)

;; Focus on open
(setq claude-code-ide-focus-on-open t)

;; Use side window
(setq claude-code-ide-use-side-window t)
```

### For Emacs Context
```elisp
;; Add custom system prompt
(setq claude-code-ide-system-prompt
      "You are a Python expert. Always use type hints and docstrings.")

;; Or for general coding
(setq claude-code-ide-system-prompt
      "Be concise. Provide code examples. Use best practices.")
```

---

## Troubleshooting Quick Links

| Problem | Solution |
|---------|----------|
| "Claude Code CLI not available" | Run `M-x claude-code-ide-check-status` |
| "No Claude Code session" | Run `M-x claude-code-ide` first |
| Window not appearing | Set `claude-code-ide-use-side-window` to t |
| Terminal rendering glitchy | Toggle `claude-code-ide-toggle-vterm-optimization` |
| Want to use eat instead | Set `claude-code-ide-terminal-backend` to 'eat |

See API.md section "Error Handling" for detailed explanations.

---

## External Resources

### Source Code
Located at: `/Users/sleepyeye/.emacs.d/elpaca/repos/claude-code-ide/`

### Key Files to Study
- `claude-code-ide.el` - Main logic
- `claude-code-ide-mcp.el` - MCP server
- `claude-code-ide-mcp-handlers.el` - Tool implementations

### Project Instructions
See: `/Users/sleepyeye/.emacs.d/CLAUDE.md`

---

## Documentation Statistics

### API.md
- 733 lines
- 188 functions documented
- 15+ interactive commands
- 30+ customization variables
- Complete architecture overview

### QUICK-REFERENCE.md
- 330 lines
- All interactive commands listed
- Quick lookup tables
- Common workflows
- Troubleshooting guide

### EXAMPLES.md
- 550+ lines
- 25+ code examples
- Integration patterns
- Custom function recipes
- Workflow templates

### Total Documentation
- 1600+ lines
- 50+ code examples
- Complete API coverage
- Multiple learning styles

---

## How to Use This Documentation

### If you want to...

**Send a message to Claude:**
→ See QUICK-REFERENCE.md "Prompt and Message Transmission"

**Create a custom command:**
→ See EXAMPLES.md "Advanced Usage Patterns"

**Integrate with another package:**
→ See EXAMPLES.md "Integration with Emacs Features"

**Understand internal architecture:**
→ See API.md "Key Implementation Details"

**Configure the package:**
→ See API.md "Customization Variables"

**Debug an issue:**
→ See API.md "Error Handling"

**Learn by example:**
→ See EXAMPLES.md throughout

---

## Version Info

- **Package Version**: 0.2.5 (from claude-code-ide.el)
- **MCP Protocol**: 2024-11-05
- **Total Functions**: 188
- **Documentation Generated**: 2025-11-07

---

## Next Steps

1. **Start using**: `M-x claude-code-ide`
2. **Send prompt**: `M-x claude-code-ide-send-prompt`
3. **Learn more**: Read QUICK-REFERENCE.md
4. **Customize**: Check API.md "Customization Variables"
5. **Extend**: Copy patterns from EXAMPLES.md

---

## Quick Links

- Full API Reference: [claude-code-ide-API.md](./claude-code-ide-API.md)
- Quick Reference: [claude-code-ide-QUICK-REFERENCE.md](./claude-code-ide-QUICK-REFERENCE.md)
- Code Examples: [claude-code-ide-EXAMPLES.md](./claude-code-ide-EXAMPLES.md)
- This Index: [claude-code-ide-INDEX.md](./claude-code-ide-INDEX.md)
- Source Code: `/Users/sleepyeye/.emacs.d/elpaca/repos/claude-code-ide/`

