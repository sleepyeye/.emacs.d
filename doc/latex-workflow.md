# LaTeX Writing Workflow

Optimized workflow for scientific writing with AUCTeX, latexmk, and texlab LSP.

---

## 🚀 Quick Start

### Open LaTeX Project
```
SPC p p        → Select project
SPC f p        → Find .tex file
               → Eglot + texlab starts automatically
```

### Compile Document
```
C-c C-c        → Default: LatexMk
               → Auto-detects master file
               → Compiles with XeLaTeX + BibTeX
```

### View PDF
```
C-c C-v        → Forward search to Sioyek
               → Opens PDF at cursor location
```

---

## 📍 Navigation

### Jump Between Sections
```latex
\chapter{Introduction}

\section{Background}  ← cursor here

\section{Methods}

]f             → Jump to next section (tree-sitter)
[f             → Jump to previous section
```

### Search LaTeX Sections
```
SPC s L        → Search in LaTeX files only
               → Enter: "Methods"
               → Finds \section{Methods}

# Or use specialized function:
M-x sleepy/search-latex-sections
               → Finds all \chapter, \section, etc.
```

### Navigate Citations & References
```latex
See \ref{fig:results} for details.
     ^cursor on ref

SPC c d        → Jump to \label{fig:results}
C-o            → Back

\cite{smith2020}
      ^cursor

SPC c d        → Jump to bibliography entry
```

### Symbol Search
```latex
\newcommand{\vect}[1]{\mathbf{#1}}
            ^cursor

SPC s y        → Find all uses of \vect
```

---

## ✏️ Editing

### Smart Text Objects
```latex
\section{Introduction}
This is a paragraph with important content.
^cursor anywhere in section

cif            → Change entire section content
yif            → Copy section
dif            → Delete section content
```

### Math Mode Editing
```latex
$E = mc^2$
 ^cursor inside

ci$            → Change math content
di$            → Delete math
yi$            → Copy math
```

### Environment Manipulation
```latex
\begin{equation}
  E = mc^2
\end{equation}

# Cursor anywhere inside
cie            → Change environment content
dae            → Delete entire environment
yae            → Copy entire environment
```

### Surround with Commands
```latex
important text
^select in visual mode

S\              → Prompt for command
                → Type "textbf"
                → Result: \textbf{important text}

# Or for existing commands:
\emph{text}
^cursor on emph

cs{           → Change surrounding \emph to...
              → Type new command
```

### Smart Quotes (Electric Quote Mode)
```latex
# Type straight quotes, get curly quotes automatically:
"Hello"        → "Hello"  (curly quotes)
               → (Disabled in math/verbatim)
```

### Auto-close Braces
```latex
# Electric pair mode auto-closes:
\frac{         → \frac{|}  (cursor inside)
               → Type numerator, then Tab to skip
```

---

## 🔧 LSP Features (texlab)

### Jump to Definition
```latex
\input{chapters/intro}
       ^cursor

SPC c d        → Open chapters/intro.tex
```

### Find References
```latex
\label{eq:einstein}
       ^cursor

SPC c D        → Find all \ref{eq:einstein}
```

### View Documentation
```latex
\includegraphics[width=0.8\textwidth]{figure}
                 ^cursor on width

SPC c h        → Show LaTeX documentation
               → Explains parameter usage
```

### Format LaTeX
```latex
# Messy spacing
\begin{equation}E=mc^2\end{equation}

SPC c f        → Format with latexindent
               → Proper spacing, alignment
```

### Symbol Completion
```latex
# In math mode, type:
\alp<Tab>      → \alpha
\bet<Tab>      → \beta

# Greek letters, operators, etc.
# Corfu shows completions automatically
```

---

## 📝 Compilation & Viewing

### Compile with LatexMk
```
C-c C-c        → LatexMk (default)
               → Runs XeLaTeX + BibTeX automatically
               → Multi-pass compilation
               → Handles citations, references
```

### Forward Search (SyncTeX)
```latex
This is important text.
^cursor here

C-c C-v        → Opens PDF in Sioyek
               → Highlights corresponding location
```

### View Compilation Log
```
C-c C-l        → Show LaTeX log
               → Jump to errors/warnings
```

### Clean Auxiliary Files
```
C-c C-c        → Clean
               → Removes .aux, .log, .out, etc.
```

---

## 🔍 Search & Find

### Search LaTeX Files Only
```
SPC s L        → "Search in LaTeX files: "
               → Enter term
               → Only searches .tex files
```

### Find Sections/Chapters
```
M-x sleepy/search-latex-sections
               → Finds all \chapter, \section, \subsection
               → Jump to section with Enter
```

### Find Undefined References
```
SPC s p        → "undefined reference"
               → Find missing \label definitions
```

### Search Equations
```
SPC s p        → "\\begin{equation}"
               → Find all equations
               → Or use imenu: SPC s i
```

### Find Citations
```
SPC s p        → "\\cite"
               → All citations in project
C-.            → E → Export to grep
               → Batch edit citations
```

---

## 📚 Bibliography Management (RefTeX)

### Insert Citation
```latex
# In text:
C-c [          → RefTeX citation menu
               → Search bibliography
               → Select reference
               → Inserts \cite{key}
```

### View Bibliography
```
C-c &          → Show bibliography
               → Browse .bib entries
```

### Jump to Bibliography Entry
```latex
\cite{smith2020}
      ^cursor

SPC c d        → Jump to bibliography .bib file
```

---

## 💡 Common Workflows

### 1. Writing a New Section
```
# Navigate to insertion point
SPC s i        → See document structure
               → Navigate with j/k

# Insert section
\section{New Section}
C-c C-e        → Insert environment
               → Select "itemize", "equation", etc.

# Compile and view
C-c C-c        → Compile
C-c C-v        → View PDF
```

### 2. Reorganize Sections
```latex
\section{Methods}
...content...
^select entire section in visual mode

d              → Cut section
]f             → Jump to next section
P              → Paste before

C-c C-c        → Recompile
```

### 3. Fix All Citations
```
SPC s p        → "\cite"
C-.            → E → Export to grep buffer
               → Edit all citations at once
C-c C-c        → Apply changes
SPC c o        → Organize bibliography (if using LSP)
```

### 4. Add Equation with Label
```latex
# Type equation
C-c C-e        → Insert environment
               → Choose "equation"
\begin{equation}
  \label{eq:|}  ← cursor here
  E = mc^2
\end{equation}

# Reference it later
\ref{eq:        → Tab completion shows labels
```

### 5. Review and Compile Loop
```
# Edit LaTeX
C-c C-c        → Compile (LatexMk)
C-c C-v        → View (forward search)
               → Check PDF
C-o            → Back to LaTeX
               → Continue editing
```

---

## 🎨 Snippets (Yasnippet)

### Common LaTeX Snippets
```
SPC i s        → Insert snippet
               → Choose from:
               → "figure", "table", "equation", etc.

# Or type abbreviation + Tab:
fig<Tab>       → \begin{figure} template
eq<Tab>        → \begin{equation} template
item<Tab>      → \begin{itemize} template
```

### Create Custom Snippet
```
SPC i n        → New snippet
               → Define trigger word
               → Write template with $1, $2 (tab stops)
SPC i v        → Visit snippet file to edit
```

---

## 📊 Math Mode Tips

### Quick Math Symbols
```latex
# In math mode:
\alpha, \beta, \gamma, ...     (Greek)
\sum, \int, \prod              (Operators)
\frac{a}{b}                    (Fractions)
\sqrt{x}                       (Roots)

# Subscript/superscript auto-braces:
x_           → x_{}   (cursor inside)
x^           → x^{}
```

### Math Text Objects
```latex
$E = mc^2$
 ^cursor

ci$           → Change inside math
ca$           → Change around (including $)
di$           → Delete inside
```

---

## 🔄 Multi-file Projects

### Master File
```latex
% In main.tex:
% TeX-master: t

% In chapter1.tex:
% TeX-master: "../main.tex"

# Now C-c C-c from chapter1.tex compiles main.tex
```

### Include Files
```latex
\input{chapters/intro}
       ^cursor

SPC c d        → Open intro.tex
C-o            → Back to main file
```

---

## ⚡ Performance Tips

### Large Documents
```
# Use imenu for navigation instead of scrolling
SPC s i        → Quick section jump

# Split into multiple files
\include{chapter1}
\include{chapter2}
```

### Slow Compilation
```
# Use draft mode
\documentclass[draft]{article}

# Or compile selection only
C-c C-r        → Compile region
```

---

## 📝 Quick Reference

| Task | Keybinding | Description |
|------|-----------|-------------|
| Compile | `C-c C-c` | LatexMk (XeLaTeX + BibTeX) |
| View PDF | `C-c C-v` | Forward search to Sioyek |
| Insert environment | `C-c C-e` | itemize, equation, etc. |
| Insert citation | `C-c [` | RefTeX citation menu |
| Jump to definition | `SPC c d` | Labels, citations, includes |
| Format | `SPC c f` | latexindent |
| Search LaTeX files | `SPC s L` | Only .tex files |
| Document structure | `SPC s i` | Imenu sections |
| Next/prev section | `]f` / `[f` | Navigate sections |

### Math Mode
| Keybinding | Description |
|-----------|-------------|
| `ci$` | Change inside $ $ |
| `ca$` | Change around (including $) |
| `ys{motion}$` | Surround with $ |

### Text Objects
| Keybinding | Description |
|-----------|-------------|
| `cif` | Change section content |
| `cie` | Change environment content |
| `dae` | Delete entire environment |

---

See also:
- `keybindings.md` - Complete keybinding reference
- `cheatsheet.md` - Quick reference card
