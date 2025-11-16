# Claude Code IDE - Complete API Reference

This document provides a comprehensive guide to all available interactive functions and their usage in the claude-code-ide Emacs package.

**Package Location**: `/Users/sleepyeye/.emacs.d/elpaca/repos/claude-code-ide/`
**Total Functions Defined**: 188

---

## Main Interactive Commands (User-Facing)

These are the primary commands that users interact with directly. They are marked with `;;;###autoload`.

### Session Management

#### `claude-code-ide`
**File**: `claude-code-ide.el` (Line 966-969)
```elisp
(defun claude-code-ide ()
  "Run Claude Code in a terminal for the current project or directory."
  (interactive)
  (claude-code-ide--start-session))
```
**Usage**: `M-x claude-code-ide`
**Purpose**: Start a new Claude Code session for the current project
**Returns**: None (side effect: creates terminal buffer and MCP server)
**Keybinding Example**: Can be bound to `C-c C-'` (see ai.el)

#### `claude-code-ide-resume`
**File**: `claude-code-ide.el` (Line 972-977)
```elisp
(defun claude-code-ide-resume ()
  "Resume Claude Code in a terminal for the current project or directory.
This starts Claude with the -r (resume) flag to continue the previous
conversation."
  (interactive)
  (claude-code-ide--start-session nil t))
```
**Usage**: `M-x claude-code-ide-resume`
**Purpose**: Resume the previous Claude conversation (uses `-r` flag)
**Parameters**: None (uses `(continue=nil, resume=t)`)
**Returns**: None
**CLI Flag**: `-r`

#### `claude-code-ide-continue`
**File**: `claude-code-ide.el` (Line 980-985)
```elisp
(defun claude-code-ide-continue ()
  "Continue the most recent Claude Code conversation in the current directory.
This starts Claude with the -c (continue) flag to continue the most recent
conversation in the current directory."
  (interactive)
  (claude-code-ide--start-session t))
```
**Usage**: `M-x claude-code-ide-continue`
**Purpose**: Continue the most recent conversation (uses `-c` flag)
**Parameters**: None (uses `(continue=t, resume=nil)`)
**Returns**: None
**CLI Flag**: `-c`

### Prompt and Message Transmission

#### `claude-code-ide-send-prompt`
**File**: `claude-code-ide.el` (Line 1103-1118)
```elisp
(defun claude-code-ide-send-prompt (&optional prompt)
  "Send a prompt to the Claude Code terminal.
When called interactively, reads a prompt from the minibuffer.
When called programmatically, sends the given PROMPT string."
  (interactive)
  (let ((buffer-name (claude-code-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (let ((prompt-to-send (or prompt (read-string "Claude prompt: "))))
          (when (not (string-empty-p prompt-to-send))
            (with-current-buffer buffer
              (claude-code-ide--terminal-send-string prompt-to-send)
              (sit-for 0.1)
              (claude-code-ide--terminal-send-return))
            (claude-code-ide-debug "Sent prompt to Claude Code: %s" prompt-to-send)))
      (user-error "No Claude Code session for this project"))))
```
**Usage**: 
- Interactive: `M-x claude-code-ide-send-prompt` → prompts in minibuffer
- Programmatic: `(claude-code-ide-send-prompt "Your prompt here")`

**Parameters**:
- `prompt` (optional string): The prompt text to send. If omitted, reads from minibuffer
  
**Returns**: None (side effect: sends text to terminal buffer)
**Important**: 
- Uses `claude-code-ide--terminal-send-string` which handles both vterm and eat backends
- Includes 0.1 second delay before sending return to allow processing
- Will not send empty prompts

#### `claude-code-ide-insert-at-mentioned`
**File**: `claude-code-ide.el` (Line 1054-1063)
```elisp
(defun claude-code-ide-insert-at-mentioned ()
  "Insert selected text into Claude prompt."
  (interactive)
  (if-let* ((project-dir (claude-code-ide-mcp--get-buffer-project))
            (session (claude-code-ide-mcp--get-session-for-project project-dir))
            (client (claude-code-ide-mcp-session-client session)))
      (progn
        (claude-code-ide-mcp-send-at-mentioned)
        (claude-code-ide-debug "Sent selection to Claude Code"))
    (user-error "Claude Code is not connected.  Please start Claude Code first")))
```
**Usage**: `M-x claude-code-ide-insert-at-mentioned`
**Purpose**: Send currently selected text to Claude via MCP
**Parameters**: None (uses current buffer selection)
**Returns**: None
**Prerequisites**: 
- Claude Code session must be running
- Current buffer must be part of a project
- Must have active selection
**Internally Calls**: `claude-code-ide-mcp-send-at-mentioned`

### Window and Buffer Management

#### `claude-code-ide-switch-to-buffer`
**File**: `claude-code-ide.el` (Line 1017-1029)
```elisp
(defun claude-code-ide-switch-to-buffer ()
  "Switch to the Claude Code buffer for the current project.
If the buffer is not visible, display it in the configured side window.
If the buffer is already visible, switch focus to it."
  (interactive)
  (let ((buffer-name (claude-code-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (if-let ((window (get-buffer-window buffer)))
            (select-window window)
          (claude-code-ide--display-buffer-in-side-window buffer))
      (user-error "No Claude Code session for this project.  Use M-x claude-code-ide to start one"))))
```
**Usage**: `M-x claude-code-ide-switch-to-buffer`
**Purpose**: Focus or display the Claude Code terminal buffer
**Parameters**: None
**Returns**: None
**Behavior**:
- If buffer visible: selects its window
- If buffer exists but hidden: displays it in side window
- If buffer doesn't exist: errors

#### `claude-code-ide-toggle`
**File**: `claude-code-ide.el` (Line 1121-1129)
```elisp
(defun claude-code-ide-toggle ()
  "Toggle visibility of Claude Code window for the current project."
  (interactive)
  (let* ((working-dir (claude-code-ide--get-working-directory))
         (buffer-name (claude-code-ide--get-buffer-name))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (claude-code-ide--toggle-existing-window buffer working-dir)
      (user-error "No Claude Code session for this project"))))
```
**Usage**: `M-x claude-code-ide-toggle`
**Purpose**: Toggle visibility of Claude window for current project
**Parameters**: None
**Returns**: None

#### `claude-code-ide-toggle-recent`
**File**: `claude-code-ide.el` (Line 1132-1163)
```elisp
(defun claude-code-ide-toggle-recent ()
  "Toggle visibility of the most recent Claude Code window.
If any Claude window is visible, hide all of them.
If no Claude windows are visible, show the most recently accessed one."
  (interactive)
  (let ((found-visible nil))
    (maphash (lambda (directory _process)
               (let* ((buffer-name (funcall claude-code-ide-buffer-name-function directory))
                      (buffer (get-buffer buffer-name)))
                 (when (and buffer
                            (buffer-live-p buffer)
                            (get-buffer-window buffer))
                   (claude-code-ide--toggle-existing-window buffer directory)
                   (setq found-visible t))))
             claude-code-ide--processes)

    (cond
     (found-visible
      (message "Closed all Claude Code windows"))
     ((and claude-code-ide--last-accessed-buffer
           (buffer-live-p claude-code-ide--last-accessed-buffer))
      (claude-code-ide--display-buffer-in-side-window claude-code-ide--last-accessed-buffer)
      (message "Opened most recent Claude Code session"))
     (t
      (user-error "No recent Claude Code session to toggle")))))
```
**Usage**: `M-x claude-code-ide-toggle-recent`
**Purpose**: Toggle visibility of most recently used Claude window
**Parameters**: None
**Returns**: None
**Smart Behavior**:
- If any Claude window visible: hides all
- Otherwise: shows most recently accessed window
- Tracks last accessed buffer across all sessions

#### `claude-code-ide-list-sessions`
**File**: `claude-code-ide.el` (Line 1032-1051)
```elisp
(defun claude-code-ide-list-sessions ()
  "List all active Claude Code sessions and switch to selected one."
  (interactive)
  (claude-code-ide--cleanup-dead-processes)
  (let ((sessions '()))
    (maphash (lambda (directory _)
               (push (cons (abbreviate-file-name directory)
                           directory)
                     sessions))
             claude-code-ide--processes)
    (if sessions
        (let ((choice (completing-read "Switch to Claude Code session: "
                                       sessions nil t)))
          (when choice
            (let* ((directory (alist-get choice sessions nil nil #'string=))
                   (buffer-name (funcall claude-code-ide-buffer-name-function directory)))
              (if-let ((buffer (get-buffer buffer-name)))
                  (claude-code-ide--display-buffer-in-side-window buffer)
                (user-error "Buffer for session %s no longer exists" choice)))))
      (claude-code-ide-log "No active Claude Code sessions"))))
```
**Usage**: `M-x claude-code-ide-list-sessions`
**Purpose**: Interactively select from all running Claude sessions
**Parameters**: None
**Returns**: None
**Features**:
- Uses completing-read for selection
- Shows abbreviated directory names
- Cleans up dead processes before listing
- Switches to selected session's buffer

### Session Control

#### `claude-code-ide-stop`
**File**: `claude-code-ide.el` (Line 1001-1013)
```elisp
(defun claude-code-ide-stop ()
  "Stop the Claude Code session for the current project or directory."
  (interactive)
  (let* ((working-dir (claude-code-ide--get-working-directory))
         (buffer-name (claude-code-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (progn
          (kill-buffer buffer)
          (claude-code-ide-log "Stopping Claude Code in %s..."
                               (file-name-nondirectory (directory-file-name working-dir))))
      (claude-code-ide-log "No Claude Code session is running in this directory"))))
```
**Usage**: `M-x claude-code-ide-stop`
**Purpose**: Stop the Claude Code session for current project
**Parameters**: None
**Returns**: None
**Note**: Killing the buffer triggers process sentinel which handles cleanup

### Terminal Interaction

#### `claude-code-ide-send-escape`
**File**: `claude-code-ide.el` (Line 1066-1073)
```elisp
(defun claude-code-ide-send-escape ()
  "Send escape key to the Claude Code terminal buffer for the current project."
  (interactive)
  (let ((buffer-name (claude-code-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (claude-code-ide--terminal-send-escape))
      (user-error "No Claude Code session for this project"))))
```
**Usage**: `M-x claude-code-ide-send-escape` or `C-<escape>`
**Purpose**: Send escape key to Claude terminal (e.g., to exit prompts)
**Parameters**: None
**Returns**: None
**Backend Support**: Works with both vterm and eat

#### `claude-code-ide-insert-newline`
**File**: `claude-code-ide.el` (Line 1076-1087)
```elisp
(defun claude-code-ide-insert-newline ()
  "Send newline (backslash + return) to the Claude Code terminal buffer for the current project.
This simulates typing backslash followed by Enter, which Claude Code interprets as a newline."
  (interactive)
  (let ((buffer-name (claude-code-ide--get-buffer-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (claude-code-ide--terminal-send-string "\\")
          (sit-for 0.1)
          (claude-code-ide--terminal-send-return))
      (user-error "No Claude Code session for this project"))))
```
**Usage**: `M-x claude-code-ide-insert-newline` or `S-<return>`
**Purpose**: Insert a newline in multi-line Claude prompts
**Parameters**: None
**Returns**: None
**Implementation**: Sends `\` + 0.1s delay + `return`

### Status and Debugging

#### `claude-code-ide-check-status`
**File**: `claude-code-ide.el` (Line 988-998)
```elisp
(defun claude-code-ide-check-status ()
  "Check Claude Code CLI status."
  (interactive)
  (claude-code-ide--detect-cli)
  (if claude-code-ide--cli-available
      (let ((version-output
             (with-temp-buffer
               (call-process claude-code-ide-cli-path nil t nil "--version")
               (buffer-string))))
        (claude-code-ide-log "Claude Code CLI version: %s" (string-trim version-output)))
    (claude-code-ide-log "Claude Code is not installed.")))
```
**Usage**: `M-x claude-code-ide-check-status`
**Purpose**: Verify Claude Code CLI is installed and get version
**Parameters**: None
**Returns**: None (outputs to log)
**Command**: Internally runs `claude --version`

#### `claude-code-ide-toggle-vterm-optimization`
**File**: `claude-code-ide.el` (Line 1090-1100)
```elisp
(defun claude-code-ide-toggle-vterm-optimization ()
  "Toggle vterm rendering optimization.
This command switches the advanced rendering optimization on or off.
Use this to balance between visual smoothness and raw responsiveness."
  (interactive)
  (setq claude-code-ide-vterm-anti-flicker
        (not claude-code-ide-vterm-anti-flicker))
  (message "Vterm rendering optimization %s"
           (if claude-code-ide-vterm-anti-flicker
               "enabled (smoother display with minimal latency)"
             "disabled (direct rendering, maximum responsiveness)")))
```
**Usage**: `M-x claude-code-ide-toggle-vterm-optimization`
**Purpose**: Toggle anti-flicker rendering for vterm (only affects vterm backend)
**Parameters**: None
**Returns**: None (shows message)
**Affects**: `claude-code-ide-vterm-anti-flicker` custom variable

---

## Internal API (Helper Functions)

These functions power the interactive commands and MCP integration.

### Session Core Functions

#### `claude-code-ide--start-session`
**File**: `claude-code-ide.el` (Line 864-963)
```elisp
(defun claude-code-ide--start-session (&optional continue resume)
  "Start a Claude Code session for the current project.
If CONTINUE is non-nil, start Claude with the -c (continue) flag.
If RESUME is non-nil, start Claude with the -r (resume) flag."
```
**Parameters**:
- `continue` (optional boolean): Add `-c` flag
- `resume` (optional boolean): Add `-r` flag

**Returns**: None (creates session with process, buffer, MCP server)
**Key Responsibilities**:
1. Checks CLI availability
2. Cleans up dead processes
3. Detects existing sessions and toggles window
4. Starts MCP server
5. Creates terminal session
6. Sets up process sentinel
7. Displays buffer in side window

#### `claude-code-ide--create-terminal-session`
**File**: `claude-code-ide.el` (Line 783-862)
```elisp
(defun claude-code-ide--create-terminal-session (buffer-name working-dir port continue resume session-id)
  "Create a new terminal session for Claude Code."
```
**Parameters**:
- `buffer-name` (string): Name for terminal buffer
- `working-dir` (string): Working directory path
- `port` (integer): MCP server port
- `continue` (boolean): Continue flag
- `resume` (boolean): Resume flag
- `session-id` (string): Unique session ID

**Returns**: `(buffer . process)` cons cell
**Supports**: Both vterm and eat terminal backends
**Environment Variables Set**:
- `CLAUDE_CODE_SSE_PORT`: MCP port
- `ENABLE_IDE_INTEGRATION=true`
- `TERM_PROGRAM=emacs`
- `FORCE_CODE_TERMINAL=true`

#### `claude-code-ide--build-claude-command`
**File**: `claude-code-ide.el` (Line 689-745)
```elisp
(defun claude-code-ide--build-claude-command (&optional continue resume session-id)
  "Build the Claude command with optional flags."
```
**Parameters**:
- `continue` (optional): Add `-c` flag
- `resume` (optional): Add `-r` flag
- `session-id` (optional): For MCP server URL path

**Returns**: Command string ready for shell execution
**Flags Added**:
- `-d`: Debug mode (if `claude-code-ide-cli-debug`)
- `-r`: Resume (if `resume`)
- `-c`: Continue (if `continue`)
- `--append-system-prompt`: Emacs context + custom prompt
- `--mcp-config`: JSON config with tools
- `--allowedTools`: Tool restrictions

**Default System Prompt Includes**:
```
"IMPORTANT: Connected to Emacs via claude-code-ide.el integration. 
Emacs uses mixed coordinates: Lines: 1-based, Columns: 0-based. 
Available: xref (LSP), tree-sitter, imenu, project.el, flycheck/flymake diagnostics. 
Context-aware with automatic project/file/selection tracking."
```

### Terminal Communication

#### `claude-code-ide--terminal-send-string`
**File**: `claude-code-ide.el` (Line 410-419)
```elisp
(defun claude-code-ide--terminal-send-string (string)
  "Send STRING to the terminal in the current buffer."
```
**Parameters**: `string` (text to send)
**Returns**: None
**Backend Support**:
- **vterm**: Uses `vterm-send-string`
- **eat**: Uses `eat-term-send-string`

#### `claude-code-ide--terminal-send-escape`
**File**: `claude-code-ide.el` (Line 421-430)
**Parameters**: None
**Returns**: None

#### `claude-code-ide--terminal-send-return`
**File**: `claude-code-ide.el` (Line 432-441)
**Parameters**: None
**Returns**: None

### Project and Directory Management

#### `claude-code-ide--get-working-directory`
**File**: `claude-code-ide.el` (Line 526-530)
```elisp
(defun claude-code-ide--get-working-directory ()
  "Get the current working directory (project root or current directory)."
```
**Returns**: Project root (via `project-current`) or `default-directory`

#### `claude-code-ide--get-buffer-name`
**File**: `claude-code-ide.el` (Line 532-536)
```elisp
(defun claude-code-ide--get-buffer-name (&optional directory)
  "Get the buffer name for the Claude Code session in DIRECTORY."
```
**Returns**: Buffer name (default format: `*claude-code[project-name]*`)
**Customizable**: Via `claude-code-ide-buffer-name-function`

#### `claude-code-ide--get-process`
**File**: `claude-code-ide.el` (Line 538-541)
```elisp
(defun claude-code-ide--get-process (&optional directory)
  "Get the Claude Code process for DIRECTORY or current working directory."
```
**Returns**: Process object or nil

---

## MCP Functions (Message Context Protocol)

These functions handle MCP server communication and Emacs integration.

### MCP Session Management

#### `claude-code-ide-mcp-send-at-mentioned`
**File**: Found referenced in `claude-code-ide-mcp-handlers.el`
**Purpose**: Send current selection to Claude via MCP
**Implementation**: Internal MCP message handling

### MCP Tool Functions

#### `claude-code-ide-mcp-handle-open-file`
**File**: `claude-code-ide-mcp-handlers.el`
**Purpose**: Tool to open/switch to files in Emacs

#### `claude-code-ide-mcp-handle-get-current-selection`
**File**: `claude-code-ide-mcp-handlers.el`
**Purpose**: Get current buffer selection for Claude

#### `claude-code-ide-mcp-handle-get-open-editors`
**File**: `claude-code-ide-mcp-handlers.el`
**Purpose**: Get list of open buffers/editors

#### `claude-code-ide-mcp-handle-open-diff`
**File**: `claude-code-ide-mcp-handlers.el`
**Purpose**: Open ediff view for file changes

#### `claude-code-ide-mcp-handle-save-document`
**File**: `claude-code-ide-mcp-handlers.el`
**Purpose**: Save a document/buffer

#### `claude-code-ide-mcp-handle-close-tab`
**File**: `claude-code-ide-mcp-handlers.el`
**Purpose**: Close a buffer/tab

#### `claude-code-ide-mcp-handle-get-diagnostics`
**File**: `claude-code-ide-mcp-handlers.el`
**Purpose**: Get diagnostics (via flycheck/flymake)

---

## Customization Variables

### Session Configuration

- `claude-code-ide-cli-path` (string): Path to Claude CLI executable (default: "claude")
- `claude-code-ide-cli-debug` (boolean): Enable debug mode (default: nil)
- `claude-code-ide-cli-extra-flags` (string): Additional CLI flags (default: "")
- `claude-code-ide-system-prompt` (string): Custom system prompt (default: nil)

### Window Display

- `claude-code-ide-use-side-window` (boolean): Use side window (default: t)
- `claude-code-ide-window-side` (symbol): Window side - left|right|top|bottom (default: 'right)
- `claude-code-ide-window-width` (integer): Width for left/right windows (default: 90)
- `claude-code-ide-window-height` (integer): Height for top/bottom windows (default: 20)
- `claude-code-ide-focus-on-open` (boolean): Focus window when opening (default: t)

### Display Behavior

- `claude-code-ide-focus-claude-after-ediff` (boolean): Focus Claude after ediff (default: t)
- `claude-code-ide-show-claude-window-in-ediff` (boolean): Show Claude window during ediff (default: t)
- `claude-code-ide-use-ide-diff` (boolean): Use IDE diff viewer (default: t)
- `claude-code-ide-switch-tab-on-ediff` (boolean): Switch tab when opening ediff (default: t)

### Terminal Backend

- `claude-code-ide-terminal-backend` (symbol): vterm|eat (default: 'vterm)
- `claude-code-ide-vterm-anti-flicker` (boolean): Enable flicker reduction (default: t)
- `claude-code-ide-vterm-render-delay` (number): Render delay in seconds (default: 0.005)
- `claude-code-ide-terminal-initialization-delay` (number): Initialization delay (default: 0.1)
- `claude-code-ide-eat-preserve-position` (boolean): Preserve scroll position in eat (default: t)

### MCP Tools

- `claude-code-ide-mcp-allowed-tools` (symbol|string|list): Tool restrictions
  - `'auto`: Allow all configured emacs-tools (default)
  - `nil`: Disable flag
  - String: Custom pattern
  - List: Specific tool names

---

## Transient Menu Commands

Interactive menu commands for easier access:

#### `claude-code-ide-menu`
Main menu with all operations

#### `claude-code-ide-config-menu`
Configuration submenu

#### `claude-code-ide-debug-menu`
Debugging and status submenu

---

## Important Hooks and Integration Points

### Process Sentinel
Automatically triggered when Claude process exits to handle cleanup:
```elisp
(set-process-sentinel process
                      (lambda (_proc event)
                        ;; Cleanup on exit
                        (claude-code-ide--cleanup-on-exit working-dir)))
```

### Kill Buffer Hook
Added to detect buffer kills and trigger cleanup

### Emacs Exit Hook
Cleanup all sessions on Emacs shutdown

### MCP Server Integration
- Websocket server on dynamically allocated port
- JSON-RPC message handling
- Tool request/response processing
- Session state management

---

## How to Start Claude with Prompts

### Method 1: Use `claude-code-ide-send-prompt`
```elisp
;; Interactive: reads from minibuffer
(claude-code-ide-send-prompt)

;; Programmatic: send specific prompt
(claude-code-ide-send-prompt "Analyze this file and explain what it does")
```

### Method 2: Start with Continue/Resume then Send Prompt
```elisp
;; Start Claude and continue recent conversation
(claude-code-ide-continue)
;; After Claude starts, send prompt
(claude-code-ide-send-prompt "What was the last issue we were discussing?")
```

### Method 3: Start Fresh and Send Prompt
```elisp
;; Start fresh Claude session
(claude-code-ide)
;; After Claude starts, send prompt
(claude-code-ide-send-prompt "Help me refactor this function")
```

### Method 4: Use Interactive Menu
```
M-x claude-code-ide-menu → s (start) → p (send prompt)
```

---

## Key Implementation Details

### Working Directory Detection
- Uses `project.el` to find project root
- Falls back to `default-directory` if no project found
- Critical for multi-project workspace support

### Terminal Backend Abstraction
All terminal operations go through wrapper functions that support both vterm and eat:
- `claude-code-ide--terminal-send-string`
- `claude-code-ide--terminal-send-escape`
- `claude-code-ide--terminal-send-return`

### Anti-Flicker Optimization (vterm only)
- Smart renderer detects rapid redraw sequences
- Batches updates using timer
- Only applies to Claude buffers
- Configurable via `claude-code-ide-vterm-anti-flicker`

### MCP Server Lifecycle
1. Started when session begins
2. Port dynamically allocated
3. Lockfile created for CLI discovery
4. Closed when session ends
5. All tools registered with MCP

### System Prompt Integration
Always includes Emacs-specific prompt explaining:
- Mixed coordinate system (1-based lines, 0-based columns)
- Available tools (xref, tree-sitter, imenu, etc.)
- IDE integration features

---

## Error Handling

### CLI Not Available
```
User Error: "Claude Code CLI not available. Please install it and ensure it's in PATH"
```

### No Active Session
```
User Error: "Claude Code is not connected. Please start Claude Code first"
```

### No Buffer for Session
```
User Error: "No Claude Code session for this project"
```

---

## Performance Optimizations

1. **Buffer-local caching**: Project lookups cached per buffer
2. **Lazy loading**: Features loaded on demand
3. **Batched updates**: Terminal updates batched to reduce flicker
4. **Incremental scanning**: Processes reused when possible
5. **Smart polling**: Selection changes debounced

---

## Files Reference

| File | Purpose |
|------|---------|
| `claude-code-ide.el` | Main entry point, session management, terminal creation |
| `claude-code-ide-mcp.el` | WebSocket server, JSON-RPC, session tracking |
| `claude-code-ide-mcp-handlers.el` | MCP tool implementations (file ops, diagnostics, etc.) |
| `claude-code-ide-mcp-server.el` | HTTP-based MCP tools server |
| `claude-code-ide-mcp-http-server.el` | HTTP transport implementation |
| `claude-code-ide-emacs-tools.el` | Emacs tools: xref, project info, imenu |
| `claude-code-ide-diagnostics.el` | Flycheck/flymake integration |
| `claude-code-ide-transient.el` | Transient menu interface |
| `claude-code-ide-debug.el` | Debug logging utilities |
| `claude-code-ide-tests.el` | ERT test suite |

---

## Summary

The claude-code-ide package provides comprehensive Emacs integration with Claude Code through:

1. **Interactive Commands**: 15+ user-facing commands for session management and interaction
2. **Prompt Transmission**: `claude-code-ide-send-prompt` for sending messages to Claude
3. **Multi-session Support**: Track and switch between projects
4. **MCP Integration**: 15+ tools for file operations, diagnostics, and editor state
5. **Terminal Backend Support**: Both vterm and eat terminal emulators
6. **Automatic Cleanup**: Process management and session lifecycle handling
7. **Customizable Display**: Side window positioning and focus behavior
8. **Performance Optimizations**: Anti-flicker rendering, batched updates, caching

For most use cases, you'll want to use one of these functions:
- `claude-code-ide` - Start new session
- `claude-code-ide-continue` - Continue recent conversation
- `claude-code-ide-resume` - Resume previous conversation
- `claude-code-ide-send-prompt` - Send a prompt/message to Claude
- `claude-code-ide-switch-to-buffer` - Focus Claude window
