# Register System Integration Guide

## 🎯 Overview

Your registers are now fully integrated with Vertico, Consult, and Orderless for a modern completion experience!

---

## 🔑 Keybindings

| Key | Command | Description |
|-----|---------|-------------|
| `SPC r a` | `sleepy/register-add-dwim` | **Smart save** - context-aware |
| `SPC r u` | `sleepy/register-use-dwim` | **Smart use** - auto jump/insert |
| `SPC r j` | `sleepy/register-jump` | **Jump only** - filter to positions/files |
| `SPC r i` | `sleepy/register-insert` | **Insert only** - filter to text/numbers |
| `SPC r f` | `sleepy/register-save-file-position` | Explicitly save file:position |
| `SPC r l` | `consult-register` | **List all** - browse with preview |
| `SPC r s` | `consult-register-store` | Store via consult interface |
| `SPC r L` | `consult-register-load` | Load via consult interface |

---

## 🚀 Features

### 1. **Vertico Completion**

All register commands use Vertico for completion:

```
SPC r l

┌─────────────────────────────────────┐
│ Register:                           │
│ > a: "hello world"                  │  ← Text register
│   f: File: init.el:45               │  ← File register
│   m: #<marker at 234 in python.el>  │  ← Position register
│   w: [frameset]                     │  ← Window layout
└─────────────────────────────────────┘
```

- ✅ Live preview in minibuffer
- ✅ Fuzzy search with Orderless
- ✅ Cycle with `C-n` / `C-p`

---

### 2. **Filtered Commands**

#### **Jump Only** (`SPC r j`)
Shows only registers you can jump to (positions, files, layouts):

```
SPC r j

┌─────────────────────────────────────┐
│ Jump to register:                   │
│ > 1: File: auth.py:234              │
│   2: File: utils.py:67              │
│   m: #<marker at 500 in main.py>    │
└─────────────────────────────────────┘
```

**Use case**: When you have 20 registers but only want to see navigation targets.

#### **Insert Only** (`SPC r i`)
Shows only registers you can insert (text, numbers):

```
SPC r i

┌─────────────────────────────────────┐
│ Insert from register:               │
│ > t: "def __init__(self):"          │
│   c: "const x = 42"                 │
│   n: 123                            │
└─────────────────────────────────────┘
```

**Use case**: When you have many registers but only want to paste text.

---

### 3. **Enhanced Preview**

File registers show pretty format:

```
Before (default):  f: #s(sleepy-file-register "/path/to/file.py" 234)
After (enhanced):  f: File: file.py:234
```

---

### 4. **Orderless Fuzzy Search**

In `consult-register` (`SPC r l`), you can fuzzy search:

```
Type: "py 234"

Matches:
✓ 1: File: auth.py:234
✓ 2: File: test_auth.py:234
✗ 3: File: utils.py:67     (doesn't match)
```

---

### 5. **Optional: Registers in `consult-buffer`**

You can add file registers to `SPC b b` (consult-buffer)!

**To enable**, uncomment line 211 in `sleepy-register.el`:

```elisp
;; Change this line from:
;; (add-to-list 'consult-buffer-sources 'sleepy/consult--source-register-file 'append)

;; To:
(add-to-list 'consult-buffer-sources 'sleepy/consult--source-register-file 'append)
```

**Then** `SPC b b` will show:

```
┌─────────────────────────────────────┐
│ Switch to:                          │
│ > init.el                           │  ← Current buffers
│   python.el                         │
│ ─────────────────────────────────── │
│   1: auth.py:234                    │  ← File registers (NEW!)
│   2: utils.py:67                    │
│ ─────────────────────────────────── │
│   ~/projects/foo/                   │  ← Projects
└─────────────────────────────────────┘
```

**Narrow to registers only**: Press `r` while in `consult-buffer`!

---

## 📖 Workflow Examples

### Example 1: Code Review Workflow

```elisp
# You're reviewing 3 files

# File 1: auth.py:234 (bug location)
SPC r a 1  → Save to register '1'

# File 2: utils.py:67 (helper function)
SPC r a 2  → Save to register '2'

# File 3: test.py:45 (test case)
SPC r a 3  → Save to register '3'

# Now quickly jump between them:
SPC r j    → Vertico shows: 1, 2, 3
           → Type "auth" → Fuzzy matches register '1'
           → Press RET → Jump to auth.py:234!

# Or use direct jump:
SPC r u 1  → auth.py:234
SPC r u 2  → utils.py:67
SPC r u 3  → test.py:45
```

---

### Example 2: Template Insertion with Completion

```elisp
# Store common code snippets:

# Snippet 1: Python main guard
Select "if __name__ == '__main__':"
SPC r a m  → Save to register 'm'

# Snippet 2: Try-except block
Select "try:\n    pass\nexcept Exception as e:\n    pass"
SPC r a t  → Save to register 't'

# Snippet 3: Docstring template
Select '"""TODO: Add docstring."""'
SPC r a d  → Save to register 'd'

# Later, insert with fuzzy search:
SPC r i    → Vertico shows: m, t, d
           → Type "try" → Matches register 't'
           → Press RET → Inserts try-except block!
```

---

### Example 3: Multi-File Refactoring

```elisp
# Refactoring a function across 5 files

# Save locations:
models.py:123   → SPC r a 1
views.py:234    → SPC r a 2
urls.py:45      → SPC r a 3
tests.py:678    → SPC r a 4
utils.py:90     → SPC r a 5

# Jump with completion:
SPC r j
Type: "mod"   → Matches "models.py:123"
RET           → Jump there!

# Or list all:
SPC r l
Navigate with C-n/C-p
Press RET to jump
```

---

### Example 4: Window Layout Management

```elisp
# Setup perfect debugging layout:
┌──────────┬──────────┐
│ editor   │ terminal │
├──────────┼──────────┤
│ test.py  │ *help*   │
└──────────┴──────────┘

SPC r a w  → Save to register 'w'

# Later, restore layout:
SPC r j    → Shows: w: [frameset]
           → Press RET → Layout restored!
```

---

## 💡 Pro Tips

### Tip 1: Use Orderless Patterns

In `consult-register` (`SPC r l`), use space-separated patterns:

```
Type: "py 234"    → Matches: auth.py:234
Type: "file util" → Matches: File: utils.py:67
Type: "def init"  → Matches: "def __init__(self):"
```

### Tip 2: Narrow by Type

In `consult-register` (`SPC r l`), use `<` to narrow:

```
SPC r l
< f  → Show only files
< r  → Show only registers (if you enabled consult-buffer integration)
```

### Tip 3: Preview Before Jumping

`consult-register` (`SPC r l`) shows preview in minibuffer:

```
> 1: File: auth.py:234
  ↓
[Preview window shows auth.py contents around line 234]
```

Navigate with `C-n` / `C-p` to preview different registers!

### Tip 4: Combine with Embark

Press `C-.` on a register in `consult-register` for actions:

```
SPC r l
Navigate to register 'f'
C-.  → Embark actions:
     → Edit register
     → Delete register
     → Copy register name
```

### Tip 5: Persistent Sessions (Future Enhancement)

Currently registers are session-only. To persist:

```elisp
;; Add to your init.el:
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'register-alist)
```

---

## 🎨 Visual Comparison

### Before Integration:
```
M-x insert-register RET
Register: _█
(No completion, no preview, manual typing only)
```

### After Integration:
```
SPC r i
┌─────────────────────────────────────┐
│ Insert from register:               │
│ > t: "def __init__(self):"          │  ← Fuzzy searchable
│   c: "const x = 42"                 │  ← Live preview
│   m: "if __name__ == '__main__':"   │  ← Cycle with arrows
└─────────────────────────────────────┘
Type to filter: "def"  → Shows only 't'
```

---

## 🔧 Customization

### Enable File Registers in `consult-buffer`

Edit `register.el` line 211:

```elisp
;; Uncomment this line:
(add-to-list 'consult-buffer-sources 'sleepy/consult--source-register-file 'append)
```

Then `SPC b b` will show file registers alongside buffers!

### Change Register Preview Delay

Edit `completion.el`:

```elisp
(setq register-preview-delay 0.1)  ; Default: instant
;; Change to 0.5 for slower preview
```

### Customize Register Formatting

Edit `sleepy/consult-register-format` in `register.el` to change how registers display.

---

## 📊 Summary

| Feature | Status | Command |
|---------|--------|---------|
| Vertico completion | ✅ Enabled | All register commands |
| Fuzzy search | ✅ Enabled | Via Orderless |
| Live preview | ✅ Enabled | In `consult-register` |
| Filtered jump | ✅ Enabled | `SPC r j` |
| Filtered insert | ✅ Enabled | `SPC r i` |
| File register format | ✅ Enhanced | Shows "file.py:234" |
| Consult-buffer integration | 🔄 Optional | Uncomment to enable |
| Embark actions | ✅ Works | Press `C-.` in register list |

---

## 🎓 Quick Reference Card

```
SAVE
────
SPC r a  → Smart save (context-aware)
SPC r f  → File:position (explicit)
SPC r s  → Store via consult

USE
───
SPC r u  → Smart use (jump or insert)
SPC r j  → Jump only (filtered)
SPC r i  → Insert only (filtered)
SPC r L  → Load via consult

BROWSE
──────
SPC r l  → List all (with preview)
         → Type to fuzzy search
         → C-n/C-p to navigate
         → < to narrow by type
         → C-. for Embark actions
```

---

Enjoy your supercharged register system! 🚀
