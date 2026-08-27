# Emacs Quick Reference Cheatsheet

Essential keybindings at a glance.

---

## 🎯 Most Important (Must Know!)

| Keybinding | Function | Description |
|-----------|----------|-------------|
| **`SPC c d`** | Go to definition | 정의로 이동 |
| **`C-o`** | Jump back | 이전 위치로 돌아가기 ⭐⭐⭐ |
| **`SPC s p`** | Project search | ripgrep 프로젝트 검색 |
| **`SPC s b`** | Buffer search | 현재 버퍼 검색 |
| **`SPC b b`** | Switch buffer | 버퍼 전환 |
| **`SPC f f`** | Find file | 파일 찾기 |
| **`C-.`** | Embark act | 검색 결과 액션 (E=wgrep!) |

---

## 📍 Navigation

### Jump & Return
```
SPC c d        → 정의로 이동
SPC c D        → 참조 찾기
C-o            → 이전 위치
M-]            → 다음 위치
g;             → 마지막 변경 위치
```

### Move Between Functions
```
]f / [f        → 다음/이전 함수
]g / [g        → 다음/이전 클래스
```

### Marks
```
ma             → 마크 설정
'a             → 마크로 이동
```

---

## 🔍 Search

### Basic Search
```
SPC s p        → 프로젝트 전체 (ripgrep)
SPC s b        → 현재 버퍼
SPC s i        → 함수/클래스 목록 (imenu)
```

### Specialized Search
```
SPC s t        → TODO/FIXME 찾기
SPC s y        → 커서 심볼 검색
SPC s P        → Python 파일만
SPC s L        → LaTeX 파일만
```

### Embark Actions (in search results)
```
C-.            → 액션 메뉴
  E            → wgrep (일괄 편집!)
  O            → Occur buffer
```

---

## ✏️ Editing

### Text Objects (operator + i/a + object)
```
ciw            → 단어 변경
ci"            → 따옴표 안 변경
ci(            → 괄호 안 변경
cip            → 단락 변경
cif            → 함수 변경 (tree-sitter)
cia            → 인자 변경 (evil-args)
```

### Operators
```
gc{motion}     → 주석 토글 (gcc: 한 줄)
gu{motion}     → 소문자
gU{motion}     → 대문자
>{motion}      → 들여쓰기 (>>: 한 줄)
```

### Surround
```
ysiw"          → 단어를 "로 감싸기
cs"'           → "를 '로 변경
ds"            → " 제거
```

### Multiple Cursors
```
C-M-j/k        → 위/아래 줄에 커서 추가
gmm            → 모든 매칭에 커서
gmu            → 모든 커서 취소
```

---

## 🔧 LSP (Code)

```
SPC c a        → Code actions
SPC c r        → Rename
SPC c f        → Format
SPC c o        → Organize imports
SPC c h        → Documentation
SPC c s        → Symbol search
```

---

## 🗂️ Files & Buffers

### Files
```
SPC SPC        → Find file
SPC f r        → Recent files
SPC f p        → Project files
SPC -          → Dired
```

### Buffers
```
SPC b b        → Switch buffer
  w            → Narrow to workspace
SPC b d        → Close buffer
```

---

## 🪟 Windows

```
SPC w h/j/k/l  → 윈도우 이동
SPC w s/v      → 분할 (수평/수직)
SPC w d        → 닫기
C-w =          → 크기 균등
```

---

## 🏢 Workspaces

```
SPC TAB 1-9    → 워크스페이스 1-9
SPC TAB TAB    → 워크스페이스 선택
SPC p p        → 프로젝트 전환 (자동 워크스페이스)
```

---

## 🎯 Git

```
SPC g g        → Magit status
SPC g d        → Timemachine (히스토리)

# In magit:
s              → Stage
u              → Unstage
c c            → Commit
P p            → Push
```

---

## 💡 Essential Workflows

### 1. Code Navigation
```
함수명에 커서
SPC c d        → 정의로
읽기...
C-o            → 돌아오기
```

### 2. Search & Replace
```
SPC s p        → 검색어 입력
C-.            → E (Export)
편집           → 일괄 수정
C-c C-c        → 적용
```

### 3. Multi-line Edit
```
Visual mode    → 영역 선택
C-M-j/k        → 커서 추가
편집           → 동시 편집
```

### 4. Function Overview
```
SPC s i        → 함수 목록
j/k            → 선택
Enter          → 이동
```

---

## 🔤 Evil Basics

### Motion
```
h/j/k/l        → ←/↓/↑/→
w/b            → 단어 앞/뒤
0/$            → 줄 시작/끝
gg/G           → 파일 시작/끝
{/}            → 단락 이동
```

### Search
```
f{char}        → char 찾기
t{char}        → char 직전까지
;              → 반복
*              → 커서 단어 검색
```

---

## 📚 Help

```
SPC h k        → Key 설명
SPC h f        → Function 설명
SPC h v        → Variable 설명
```

---

## 🎨 Language-Specific

### Python
```
SPC c d        → Definition
SPC c r        → Rename
SPC c a        → Code actions
SPC s P        → Python 검색
]f / [f        → 함수 이동
```

### LaTeX
```
C-c C-c        → Compile (LatexMk)
C-c C-v        → View PDF (forward search)
C-c [          → Insert citation
SPC s L        → LaTeX 검색
```

---

## ⚡ Power User Tips

### Smart Combinations
```
# Function navigation loop:
SPC s i → ]f → SPC c d → C-o

# Search and edit:
SPC s y → C-. → E → edit → C-c C-c

# Multi-cursor editing:
Visual → C-M-j → edit

# Quick surround:
ysiw" → cs"' → ds'
```

### Most Used Sequences
```
1. SPC c d → C-o          (정의 보고 돌아오기)
2. SPC s p → C-. E        (검색 후 일괄 편집)
3. SPC b b → w            (워크스페이스 버퍼)
4. gcc                    (줄 주석 토글)
5. SPC g g → s → c c      (Git stage & commit)
```

---

## 📊 Key Frequency Guide

**매일 사용:**
- `SPC c d` / `C-o` (정의 이동/복귀)
- `SPC s p` (프로젝트 검색)
- `SPC b b` (버퍼 전환)
- `gcc` (주석)

**자주 사용:**
- `SPC s i` (함수 목록)
- `C-.` (embark)
- `ciw`, `ci"` (텍스트 객체)
- `ysiw"` (surround)

**가끔 사용:**
- `SPC c r` (rename)
- `SPC c a` (code actions)
- `C-M-j/k` (멀티 커서)
- `SPC TAB 1-9` (워크스페이스)

---

**Pro Tip:** `C-h B` in any context shows all available keybindings!

---

See detailed guides:
- `keybindings.md` - Complete reference
- `python-workflow.md` - Python development
- `latex-workflow.md` - LaTeX writing
