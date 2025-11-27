# Quick Reference Guide

Fast lookup for common patterns and workflows in the Emacs configuration.

## Most Used Commands

| Command | Key | Description |
|---------|-----|-------------|
| Find file | `SPC SPC` | Quick file open |
| Switch buffer | `SPC b b` | Buffer selection with preview |
| Project files | `SPC p f` | Find in project |
| Ripgrep | `SPC s r` | Search project content |
| Magit status | `SPC g g` | Git interface |
| Switch workspace | `SPC TAB TAB` | Workspace selection |

## Leader Key Map (SPC)

### Files (`SPC f`)
- `SPC f f` - Find file
- `SPC f r` - Recent files
- `SPC f s` - Save file
- `SPC f S` - Save all files
- `SPC f R` - Rename file

### Buffers (`SPC b`)
- `SPC b b` - Switch buffer
- `SPC b d` - Kill buffer
- `SPC b k` - Kill buffer
- `SPC b i` - ibuffer
- `SPC b s` - Save buffer

### Windows (`SPC w`)
- `SPC w v` - Split vertically
- `SPC w s` - Split horizontally
- `SPC w d` - Delete window
- `SPC w h/j/k/l` - Navigate windows
- `SPC w H/J/K/L` - Move windows

### Search (`SPC s`)
- `SPC s s` - Search in file (consult-line)
- `SPC s r` - Ripgrep project
- `SPC s p` - Search project files
- `SPC s i` - Imenu (code outline)
- `SPC s t` - Search TODOs

### Git (`SPC g`)
- `SPC g g` - Magit status
- `SPC g l` - Magit log
- `SPC g b` - Magit blame
- `SPC g c` - Magit clone
- `SPC g d` - Magit diff

### Project (`SPC p`)
- `SPC p p` - Switch project
- `SPC p f` - Find file in project
- `SPC p s` - Search in project
- `SPC p c` - Compile project
- `SPC p k` - Kill project buffers

### Registers (`SPC r`)
- `SPC r s` - Save position to register
- `SPC r j` - Jump to register
- `SPC r i` - Insert register content
- `SPC r l` - List all registers

### Workspaces (`SPC TAB`)
- `SPC TAB TAB` - Switch workspace
- `SPC TAB 1-9` - Quick switch to workspace 1-9
- `SPC TAB n` - Next workspace
- `SPC TAB p` - Previous workspace

### Help (`SPC h`)
- `SPC h f` - Describe function
- `SPC h v` - Describe variable
- `SPC h k` - Describe key
- `SPC h m` - Describe mode
- `SPC h i` - Info

## Evil Mode Commands

### Text Objects
- `ia`/`aa` - Inner/around arguments
- `il`/`al` - Inner/around line
- `if`/`af` - Inner/around function
- `ig`/`ag` - Inner/around class
- `ii`/`ai` - Inner/around indentation

### Operators
- `gc` - Comment/uncomment (gc + motion)
- `gx`/`gX` - Exchange regions
- `cs` - Change surround (e.g., cs"')
- `ds` - Delete surround
- `ys` - Add surround

### Navigation
- `[b`/`]b` - Previous/next buffer
- `[f`/`]f` - Previous/next function
- `[g`/`]g` - Previous/next class
- `C-o` - Jump back
- `M-]` - Jump forward (instead of C-i)
- `%` - Match bracket

### Multiple Cursors
- `C-n` - Add cursor to next match
- `C-p` - Add cursor to previous match
- `M-n` - Skip and add next
- `g z j` - Add cursor down
- `g z k` - Add cursor up

## LSP Commands

### Navigation
- `gd` - Go to definition
- `gr` - Find references
- `gi` - Go to implementation
- `gy` - Go to type definition

### Actions
- `K` - Show hover documentation
- `SPC c r` - Rename symbol
- `SPC c a` - Code actions
- `SPC c s` - Find symbols (consult-eglot)
- `SPC c f` - Format buffer

## Common Workflows

### Start New Project
```
1. SPC p p         # Switch/create project
2. Auto workspace  # Workspace created automatically
3. SPC p f         # Find files in project
```

### Git Workflow
```
1. SPC g g         # Open magit status
2. s               # Stage changes
3. c c             # Commit
4. <write message>
5. C-c C-c        # Finish commit
6. P p            # Push to remote
```

### Code Navigation
```
1. gd             # Go to definition
2. C-o            # Jump back
3. SPC s i        # Browse file symbols
4. SPC c s        # Browse project symbols
```

### Register Workflow
```
1. SPC r s a      # Save position to register 'a'
2. <work elsewhere>
3. SPC r j        # Jump back (with completion)
```

### Search and Replace
```
1. SPC s r        # Ripgrep search
2. C-c C-e        # Export to wgrep
3. <edit results>
4. C-c C-c        # Apply changes
```

## Package Management

### Elpaca Commands
- `M-x elpaca-update-all` - Update all packages
- `M-x elpaca-rebuild` - Rebuild package
- `M-x elpaca-log` - View log
- `M-x elpaca-try` - Try package temporarily

### Adding Packages
```elisp
(use-package package-name
  :ensure t
  :defer t
  :config
  ;; configuration
  )
```

## Configuration Files

### Main Files
- `init.el` - Module loading orchestration
- `early-init.el` - Pre-package optimization
- `general.el` - Keybinding definitions
- `evil.el` - Vim emulation setup

### Quick Access
- See [INDEX.md](../INDEX.md) for full navigation
- Check [module_reference.md](module_reference.md) for module details
- View [keybindings.md](keybindings.md) for complete bindings

## Tips and Tricks

### Performance
- `M-x emacs-init-time` - Check startup time
- `M-x esup` - Profile startup
- `M-x elpaca-log` - Debug package issues

### Completion
- `C-h` during completion - Show help
- `C-M-j` in Vertico - Exit with current input
- `M-RET` in Embark - Act on multiple

### Evil Mode
- `C-z` - Toggle Evil/Emacs state
- `:` - Ex commands
- `/` - Search forward
- `?` - Search backward

## External Dependencies

### Required Tools
```bash
# Package managers
brew install ripgrep fd git

# Language servers
npm install -g basedpyright
brew install llvm  # for clangd
brew install texlab

# Optional
brew install claude  # AI assistant
```

## Troubleshooting

### Common Issues
- **Slow startup**: Check `:defer` in use-package
- **Package errors**: Run `M-x elpaca-rebuild`
- **LSP not working**: Check language server installed
- **Evil state confusion**: Press `C-z` to toggle

### Reset Commands
- `M-x evil-mode` - Reset Evil
- `M-x eglot-shutdown` - Restart LSP
- `M-x persp-kill` - Reset workspace
