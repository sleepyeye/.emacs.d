# Claude Code IDE - Quick Reference Guide

## Essential Functions for Prompts and Messages

### Start Claude and Send Prompt
```elisp
;; Start fresh session
(claude-code-ide)

;; Then send a prompt
(claude-code-ide-send-prompt "Your prompt here")
```

### Continue/Resume with Prompt
```elisp
;; Continue recent conversation
(claude-code-ide-continue)
(claude-code-ide-send-prompt "Follow up question")

;; Or resume previous conversation
(claude-code-ide-resume)
(claude-code-ide-send-prompt "What were we working on?")
```

### Interactive Prompt (Minibuffer)
```elisp
M-x claude-code-ide-send-prompt
;; Then type your message when prompted
```

### Send Selection to Claude
```elisp
;; Select text in your editor, then:
M-x claude-code-ide-insert-at-mentioned

;; Or with function:
(claude-code-ide-insert-at-mentioned)
```

---

## All Interactive Commands at a Glance

| Command | Purpose | Binding |
|---------|---------|---------|
| `claude-code-ide` | Start new session | `C-c C-'` (in ai.el) |
| `claude-code-ide-continue` | Continue recent conversation | `M-x` |
| `claude-code-ide-resume` | Resume previous conversation | `M-x` |
| `claude-code-ide-stop` | Stop current session | `M-x` |
| `claude-code-ide-send-prompt` | Send a prompt/message | `M-x` |
| `claude-code-ide-insert-at-mentioned` | Send selected text | `M-x` |
| `claude-code-ide-switch-to-buffer` | Focus Claude window | `M-x` |
| `claude-code-ide-toggle` | Toggle window visibility | `M-x` |
| `claude-code-ide-toggle-recent` | Toggle most recent window | `M-x` |
| `claude-code-ide-list-sessions` | Switch between sessions | `M-x` |
| `claude-code-ide-send-escape` | Send escape key | `C-<escape>` |
| `claude-code-ide-insert-newline` | Insert newline in prompt | `S-<return>` |
| `claude-code-ide-check-status` | Check CLI status | `M-x` |

---

## The Most Important Function

### `claude-code-ide-send-prompt`

**Signature**:
```elisp
(defun claude-code-ide-send-prompt (&optional prompt)
  "Send a prompt to the Claude Code terminal.")
```

**Usage**:
```elisp
;; With argument (programmatic)
(claude-code-ide-send-prompt "Fix the bug in line 42")

;; Without argument (interactive - uses minibuffer)
(claude-code-ide-send-prompt)
;; Type: "Your message here"

;; In elisp code
(claude-code-ide-send-prompt "Help me understand this code")
```

**How it works**:
1. Gets the current session's terminal buffer
2. Sends your prompt text to the terminal
3. Waits 0.1 seconds for processing
4. Presses Enter to submit the prompt
5. Debug log shows what was sent

**Key Points**:
- Works with both vterm and eat backends
- Empty prompts are ignored (no-op)
- Requires active Claude session
- 0.1s delay is built-in (configurable via code)

---

## Programmatic Workflow Examples

### Example 1: Analyze Current File
```elisp
(defun analyze-current-file ()
  (interactive)
  (claude-code-ide)  ; Start if not running
  (sleep-for 1)      ; Wait for startup
  (let ((filename (buffer-file-name)))
    (claude-code-ide-send-prompt 
     (format "Analyze this file and explain what it does: %s" filename))))
```

### Example 2: Refactor Function
```elisp
(defun refactor-function ()
  (interactive)
  (claude-code-ide)
  (sleep-for 1)
  (claude-code-ide-send-prompt
   "Please refactor this function to improve readability and performance"))
```

### Example 3: Continue Debugging Session
```elisp
(defun continue-debug-session ()
  (interactive)
  (claude-code-ide-continue)  ; Continue recent conversation
  (sleep-for 2)              ; Let it load
  (claude-code-ide-send-prompt "I've made those changes, what should I test next?"))
```

### Example 4: Interactive Menu Flow
```elisp
;; Use transient menu (easier for users)
M-x claude-code-ide-menu
;; Then select:
;; - s for Start
;; - p for Send Prompt
```

---

## Session Management

### Check if Session is Running
```elisp
(when (claude-code-ide--get-process)
  (message "Claude is running"))
```

### Get Current Working Directory
```elisp
(claude-code-ide--get-working-directory)  ; Returns project root
```

### List All Sessions
```elisp
(claude-code-ide-list-sessions)  ; Interactive menu
```

### Switch to Claude Window
```elisp
(claude-code-ide-switch-to-buffer)
```

### Stop Session
```elisp
(claude-code-ide-stop)
```

---

## Terminal Control

### Send Escape Key
```elisp
(claude-code-ide-send-escape)
```

### Insert Newline in Multi-line Prompt
```elisp
(claude-code-ide-insert-newline)
```

### Send Raw String
```elisp
;; Low-level API (internal use)
(with-current-buffer (get-buffer "*claude-code[project]*")
  (claude-code-ide--terminal-send-string "ls -la"))
```

---

## Customization

### Set Custom System Prompt
```elisp
(setq claude-code-ide-system-prompt
      "You are a Python expert. Focus on clean, Pythonic code.")
```

### Change Window Side
```elisp
(setq claude-code-ide-window-side 'left)  ; left, right, top, bottom
```

### Enable CLI Debug Mode
```elisp
(setq claude-code-ide-cli-debug t)
```

### Change Terminal Backend
```elisp
(setq claude-code-ide-terminal-backend 'eat)  ; or 'vterm
```

### Disable Anti-Flicker (if it bothers you)
```elisp
(setq claude-code-ide-vterm-anti-flicker nil)
```

---

## CLI Flags Reference

Claude Code is started with these flags:

```bash
claude -d                                    # Debug (if enabled)
       -c                                    # Continue (for continue command)
       -r                                    # Resume (for resume command)
       --append-system-prompt "..."          # Always included
       --mcp-config "{...}"                  # MCP tools config
       --allowedTools "tool1 tool2 ..."      # Tool restrictions
```

---

## Common Workflows

### Workflow 1: Ask Claude to Analyze Code
```
1. M-x claude-code-ide                      # Start Claude
2. M-x claude-code-ide-send-prompt          # Send prompt
3. Type: "Analyze src/main.py and explain"
4. Press Enter
5. Claude analyzes and responds
```

### Workflow 2: Get Suggestions for Changes
```
1. Select text/code in editor
2. M-x claude-code-ide-insert-at-mentioned  # Send selection to Claude
3. Claude shows that selection
4. M-x claude-code-ide-send-prompt
5. Ask for suggestions: "How can I improve this?"
```

### Workflow 3: Continue Session
```
1. M-x claude-code-ide-continue             # Continue recent work
2. M-x claude-code-ide-send-prompt
3. Ask: "What's the next step?"
```

### Workflow 4: Multi-Project Work
```
1. Open project A, M-x claude-code-ide      # Start session for A
2. Open project B, M-x claude-code-ide      # Start session for B
3. M-x claude-code-ide-list-sessions        # Switch between A and B
```

---

## Important Notes

### Session Lifecycle
- Sessions are per-project (based on .git root)
- Sessions persist until manually stopped or Emacs quits
- Continuing/resuming uses last conversation in directory
- Each project can have one active session at a time

### Prompt Transmission
- Prompts go directly to Claude's terminal stdin
- 0.1 second delay before return ensures processing
- Empty prompts are ignored
- Works with both vterm and eat backends

### MCP Integration
- Automatically enabled
- Claude can request file operations, diagnostics, etc.
- Tools: xref (LSP), tree-sitter, imenu, project.el
- Can be configured via `claude-code-ide-mcp-allowed-tools`

### Coordinates
- **Lines**: 1-based (line 1 = first line)
- **Columns**: 0-based (column 0 = first character)
- Claude is informed about this in system prompt

---

## Troubleshooting

### "Claude Code CLI not available"
```elisp
(claude-code-ide-check-status)  ; Check installation
```

### "No Claude Code session for this project"
```elisp
(claude-code-ide)  ; Start a session first
```

### Claude window not appearing
```elisp
(setq claude-code-ide-use-side-window t)    ; Enable side window
(setq claude-code-ide-focus-on-open t)      ; Auto-focus on open
```

### Terminal rendering issues
```elisp
(claude-code-ide-toggle-vterm-optimization)  ; Toggle anti-flicker
```

---

## File Locations

- Source: `/Users/sleepyeye/.emacs.d/elpaca/repos/claude-code-ide/`
- Main file: `claude-code-ide.el`
- MCP handlers: `claude-code-ide-mcp-handlers.el`
- Terminal backends: Supports vterm and eat

---

## Total API Size

- **188 functions** defined across 10 files
- **15+ interactive commands** for users
- **100+ internal functions** for core logic and MCP handling

## Key Takeaway

For most use cases, you only need these 3-4 functions:

1. `claude-code-ide` - Start a session
2. `claude-code-ide-send-prompt` - Send messages/prompts
3. `claude-code-ide-continue` - Continue conversations
4. `claude-code-ide-switch-to-buffer` - Focus the Claude window
