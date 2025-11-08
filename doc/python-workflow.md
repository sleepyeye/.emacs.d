# Python Development Workflow

Optimized keybindings and workflow for Python development with Eglot (basedpyright).

---

## 🚀 Quick Start

### Opening a Python Project
```
SPC p p        → Select project (auto-creates workspace)
SPC f p        → Find Python file in project
```

### LSP Auto-starts
- Eglot automatically starts when opening `.py` files
- Uses `basedpyright-langserver` for type checking

---

## 📍 Navigation

### Jump to Definition & Back
```python
# Example: You're reading this code
result = process_data(input_file)
         ^cursor here

SPC c d        → Jump to process_data definition
C-o            → Back to original location
M-]            → Forward to definition again
```

### Find References
```python
# Find all usages of a variable/function
def calculate_score(data):
    ^cursor on function name

SPC c D        → Show all references to calculate_score
```

### Navigate Between Functions
```python
def func1():
    pass

def func2():  # ← cursor here
    pass

def func3():
    pass

]f             → Jump to func3
[f             → Jump to func1
```

### Symbol Search in Project
```python
# Find all occurrences of a class/function
class DataProcessor:
      ^cursor here

SPC s y        → Search "DataProcessor" across project
```

---

## ✏️ Editing

### Smart Text Objects
```python
def process_data(input_file, output_file, verbose=True):
    ^cursor inside function

cif            → Change entire function body
yif            → Copy entire function
dif            → Delete function body
vif            → Select function body
```

### Function Arguments
```python
result = calculate(x, y, z)
                   ^cursor on y

cia            → Change argument 'y'
L              → Jump to next argument (z)
H              → Jump to previous argument (x)
K              → Jump out of parentheses
```

### Edit Multiple Lines
```python
# Change multiple similar lines at once
data1 = process(input1)
data2 = process(input2)  # ← cursor here
data3 = process(input3)

C-M-k          → Add cursor to line above
C-M-j          → Add cursor to line below
c$             → Change from cursor to end of line
type new       → All lines change simultaneously
```

### Surround with Quotes/Brackets
```python
# Add quotes around variable
variable
^cursor

ysiw"          → "variable"
cs"'           → 'variable'
ds'            → variable

# Wrap in list
item1, item2
^select in visual mode

S]             → [item1, item2]
```

---

## 🔧 LSP Features

### Code Actions
```python
# Import missing modules, fix type errors, etc.
from typing import List

def process(data: List[str]) -> None:
    result = unknown_function(data)  # ← error here
             ^cursor

SPC c a        → Show available code actions
               → Auto-import, add type hints, etc.
```

### Rename Symbol
```python
def old_function_name(x):
    ^cursor on function name

SPC c r        → Rename everywhere in project
               → Type new name
               → All references updated!
```

### Organize Imports
```python
# Messy imports
from os import path
import sys
from typing import List
import os

SPC c o        → Organize imports
               → Sorted, grouped, duplicates removed
```

### Format Code
```python
# Badly formatted code
def   messy_function( x,y,   z ):
  return x+y+z

SPC c f        → Format buffer (using ruff/black)
               → Properly formatted!
```

### View Documentation
```python
import numpy as np

result = np.array([1, 2, 3])
            ^cursor on 'array'

SPC c h        → Open documentation buffer
               → Shows function signature, docstring
```

---

## 🔍 Search & Find

### Search in Python Files Only
```
SPC s P        → Prompt: "Search in Python files: "
               → Enter search term
               → Only searches .py files
```

### Find Function/Class Definitions
```
SPC s i        → Shows imenu of current file
               → List of all functions and classes
               → Navigate with j/k, press Enter
```

### Search TODO Comments
```python
# TODO: Optimize this algorithm
# FIXME: Handle edge case
# HACK: Temporary workaround

SPC s t        → Find all TODO/FIXME/HACK
C-.            → E → Export to grep buffer
               → Edit multiple TODOs at once!
```

### Search Current Symbol Everywhere
```python
class DataProcessor:
      ^cursor here

SPC s y        → Search "DataProcessor" in entire project
```

---

## 🧪 Testing Workflow

### Navigate to Test File
```python
# In: src/module.py
SPC p a        → projectile-toggle-between-implementation-and-test
               → Jumps to tests/test_module.py
```

### Run Tests
```
# From test file or anywhere
!pytest                    → Run all tests
!pytest tests/test_file.py → Run specific file
!pytest -v                 → Verbose output
```

---

## 💡 Common Workflows

### 1. Explore Unknown Codebase
```
SPC s i        → See structure of current file
SPC c d        → Jump to interesting function
C-o            → Back
SPC s y        → Search for related symbols
[f / ]f        → Browse between functions
```

### 2. Refactor Function Name
```
SPC c r        → Rename everywhere
SPC c f        → Format affected files
SPC g g        → Review changes in magit
c c            → Commit changes
```

### 3. Fix Type Errors
```
SPC s p        → Search for "# type: ignore"
C-.            → E → Export to grep
               → Fix all at once with wgrep
SPC c a        → Use code actions to add proper types
```

### 4. Add Missing Imports
```python
# Code with undefined names
result = pd.DataFrame(data)  # ← pd not imported

SPC c a        → Code action: "Import pandas as pd"
SPC c o        → Organize all imports
```

### 5. Extract Function
```python
# Select complex code block in visual mode
v              → Visual mode
i{             → Select inside block
SPC c a        → Code action: "Extract function"
               → (if available from LSP)
```

---

## 🎯 LSP Symbols Navigation

### Project-wide Symbol Search
```
SPC c s        → consult-eglot-symbols
               → Fuzzy search all symbols in project
               → Functions, classes, variables
```

### Find Implementation
```python
class BaseProcessor:
    def process(self):  # ← cursor on abstract method
        raise NotImplementedError

SPC c i        → Find all implementations
               → Shows derived classes
```

### Find Type Definition
```python
data: DataFrame = load_data()
      ^cursor on DataFrame

SPC c t        → Jump to DataFrame type definition
```

---

## 📊 Debugging Workflow

### Add Breakpoints
```python
# Add pdb breakpoint
import pdb; pdb.set_trace()

# Or use snippet (if configured)
SPC i s        → Insert snippet
               → Select "pdb"
```

### Navigate Stack Trace
```python
# When error occurs:
C-x `          → next-error (jump to traceback location)
```

---

## 🔄 Virtual Environment

### Activate Virtual Environment
```elisp
# Python mode will auto-detect:
# - .venv/
# - venv/
# - Poetry/Pipenv environments

# Manual activation in Emacs:
M-x pyvenv-activate
→ Select environment path
```

---

## ⚡ Performance Tips

### Large Files
```
# For large Python files (>1000 lines)
SPC s i        → Use imenu for quick navigation
               → Faster than scrolling
```

### Restart LSP if Slow
```
SPC c R        → Restart basedpyright
SPC c S        → Shutdown LSP
               → Reopen file to restart
```

---

## 📝 Quick Reference

| Task | Keybinding | Description |
|------|-----------|-------------|
| Jump to definition | `SPC c d` | Go to function/class definition |
| Back to origin | `C-o` | Return after jump |
| Find references | `SPC c D` | All usages of symbol |
| Rename | `SPC c r` | Rename everywhere |
| Code actions | `SPC c a` | Fix imports, types, etc. |
| Format | `SPC c f` | Format buffer |
| Documentation | `SPC c h` | View docs |
| Symbol search | `SPC c s` | Project-wide symbols |
| Python-only search | `SPC s P` | Search .py files |
| Function list | `SPC s i` | Imenu of file |

---

See also:
- `keybindings.md` - Complete keybinding reference
- `cheatsheet.md` - Quick reference card
