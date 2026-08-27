# Emacs Configuration - Complete Keybinding Guide

Complete reference for all keybindings in this Emacs configuration.

---

## 📍 Navigation & Jump

### Go to Definition & Back (Most Important!)

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `SPC c d` | xref-find-definitions | 정의로 이동 |
| `SPC c D` | xref-find-references | 참조 찾기 |
| **`C-o`** | better-jumper-jump-backward | **이전 위치로 돌아가기** ⭐ |
| `M-]` | better-jumper-jump-forward | 다음 위치로 이동 |

**Usage Example:**
1. 함수명에 커서 놓고 `SPC c d` → 정의로 이동
2. `C-o` → 원래 위치로 돌아옴
3. `M-]` → 다시 정의로 이동

### Evil Jump Commands

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `g;` | evil-goto-last-change | 이전 변경 위치로 이동 |
| `g,` | evil-goto-last-change-reverse | 다음 변경 위치로 이동 |
| ` `` ` | evil-jump-backward | 마지막 점프 위치 (정확한 위치) |
| `''` | evil-jump-backward | 마지막 점프한 라인의 시작 |
| `` `. `` | - | 마지막 변경 위치로 이동 |
| `` `^ `` | - | 마지막 insert 위치로 이동 |
| `gf` | find-file-at-point | 커서 아래 파일 열기 |

### Marks & Registers

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `m{a-z}` | evil-set-marker | 로컬 마크 설정 (버퍼 내) |
| `m{A-Z}` | evil-set-marker | 글로벌 마크 설정 (버퍼 간) |
| `'{mark}` | evil-goto-mark-line | 마크한 라인으로 이동 |
| `` `{mark} `` | evil-goto-mark | 마크한 정확한 위치로 이동 |

**Example:**
```
ma          → 현재 위치에 'a' 마크
...이동...
'a          → 'a' 마크한 라인으로 돌아감
`a          → 'a' 마크한 정확한 위치로
```

### Tree-sitter Navigation

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `]f` / `[f` | next/prev function | 다음/이전 함수로 이동 |
| `]g` / `[g` | next/prev class | 다음/이전 클래스로 이동 |
| `]F` / `[F` | - | 함수 끝으로 이동 |
| `]G` / `[G` | - | 클래스 끝으로 이동 |

---

## 🔍 Search & Find

### Consult Search (Leader: SPC s)

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `SPC s b` | consult-line | 현재 버퍼 내 검색 |
| `SPC s B` | consult-line-multi | 여러 버퍼 검색 |
| `SPC s p` | consult-ripgrep | 프로젝트 전체 검색 |
| `SPC s d` | consult-ripgrep-current | 현재 디렉토리 검색 |
| `SPC s i` | consult-imenu | 현재 파일 함수/클래스 목록 |
| `SPC s I` | consult-imenu-multi | 여러 파일 imenu |

### Specialized Search Functions

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `SPC s t` | search-todos | TODO/FIXME/HACK 검색 |
| `SPC s y` | search-symbol-at-point | 커서 심볼 프로젝트 전체 검색 |
| `SPC s P` | search-in-python | Python 파일만 검색 |
| `SPC s L` | search-in-latex | LaTeX 파일만 검색 |
| `SPC s C` | search-in-cpp | C/C++ 파일만 검색 |
| `SPC s x` | search-exclude-tests | 테스트 파일 제외하고 검색 |

### Embark Actions (on search results)

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `C-.` | embark-act | 액션 메뉴 표시 |
| `C-;` | embark-dwim | 자동으로 적절한 액션 실행 |
| `C-h B` | embark-bindings | 사용 가능한 키바인딩 보기 |

**In consult-ripgrep results:**
- `E` - Export to grep buffer (wgrep로 여러 파일 일괄 편집!)
- `O` - Occur buffer로 export
- `S` - 버퍼에 저장
- `W` - 파일 작업 (삭제/이동)
- `F` - Dired에서 보기

---

## ✏️ Editing & Text Objects

### Evil Text Objects

**Syntax: `{operator}{i/a}{object}`**
- operator: `c` (change), `d` (delete), `y` (yank), `v` (visual)
- i/a: `i` (inner), `a` (around/outer)

| Keybinding | Description | Example |
|-----------|-------------|---------|
| `ciw` | Change inner word | 단어 변경 |
| `ci"` | Change inside quotes | "hello" → 따옴표 안 변경 |
| `ci(`, `ci{`, `ci[` | Change inside brackets | 괄호 안 변경 |
| `cip` | Change inner paragraph | 단락 변경 |
| `cif` | Change inner function | 함수 전체 변경 (tree-sitter) |
| `cig` | Change inner class | 클래스 전체 변경 (tree-sitter) |
| `cia` | Change inner argument | 함수 인자 변경 (evil-args) |
| `cil` | Change inner line | 라인 변경 (evil-textobj-line) |
| `dap` | Delete around paragraph | 단락 삭제 |
| `yif` | Yank inner function | 함수 복사 |
| `vip` | Visual inner paragraph | 단락 선택 |

### Evil Operators

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `gc{motion}` | Comment toggle | 주석 토글 (gcc: 현재 줄) |
| `gx` | evil-exchange | 영역 교환 (첫 gx, 두 번째 gx) |
| `gu{motion}` | Lowercase | 소문자로 변환 (guiw: 단어) |
| `gU{motion}` | Uppercase | 대문자로 변환 (gUiw: 단어) |
| `>{motion}` | Indent right | 들여쓰기 (>>: 현재 줄) |
| `<{motion}` | Indent left | 내어쓰기 (<<: 현재 줄) |
| `={motion}` | Auto-indent | 자동 들여쓰기 |

### Evil Surround

| Keybinding | Function | Example |
|-----------|----------|---------|
| `ys{motion}{char}` | Add surround | `ysiw"` → word를 "word"로 |
| `cs{old}{new}` | Change surround | `cs"'` → "word"를 'word'로 |
| `ds{char}` | Delete surround | `ds"` → "word"를 word로 |
| `S{char}` | Surround selection (visual) | Visual로 선택 후 S" |

### Multiple Cursors

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `C-M-j` | evil-mc-make-cursor-move-next-line | 아래 줄에 커서 추가 |
| `C-M-k` | evil-mc-make-cursor-move-prev-line | 위 줄에 커서 추가 |
| `gmm` | evil-mc-make-all-cursors | 모든 매칭에 커서 |
| `gmn` | evil-mc-make-and-goto-next-match | 다음 매칭으로 이동하며 커서 추가 |
| `gmp` | evil-mc-make-and-goto-prev-match | 이전 매칭으로 이동하며 커서 추가 |
| `gmu` | evil-mc-undo-all-cursors | 모든 커서 취소 |

### Expand Region

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `M-=` | er/expand-region | 영역 확장 |
| `M--` | er/contract-region | 영역 축소 |

---

## 🔧 LSP/Code Actions (Eglot)

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `SPC c a` | eglot-code-actions | 코드 액션 |
| `SPC c r` | eglot-rename | 심볼 이름 변경 |
| `SPC c f` | eglot-format | 코드 포맷 |
| `SPC c o` | organize-imports | Import 정리 |
| `SPC c d` | xref-find-definitions | 정의로 이동 ⭐ |
| `SPC c D` | xref-find-references | 참조 찾기 |
| `SPC c i` | eglot-find-implementation | 구현 찾기 |
| `SPC c t` | eglot-find-typeDefinition | 타입 정의 찾기 |
| `SPC c h` | eldoc-doc-buffer | 문서 보기 |
| `SPC c s` | consult-eglot-symbols | 심볼 검색 |
| `SPC c R` | eglot-reconnect | LSP 서버 재시작 |
| `SPC c S` | eglot-shutdown | LSP 서버 종료 |

---

## 🗂️ File & Buffer Management

### File Operations (Leader: SPC f)

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `SPC SPC` | find-file | 파일 찾기 |
| `SPC f f` | find-file | 파일 찾기 |
| `SPC f r` | consult-recent-file | 최근 파일 |
| `SPC f p` | projectile-find-file | 프로젝트 파일 찾기 |
| `SPC f d` | dired-jump | Dired로 이동 |
| `SPC -` | dired-jump | Dired로 이동 (빠른) |
| `SPC f s` | save-buffer | 파일 저장 |
| `SPC f R` | rename-visited-file | 파일 이름 변경 |

### Buffer Operations (Leader: SPC b)

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `SPC b b` | consult-buffer | 버퍼 전환 ⭐ |
|           | (narrow with `w`) | 현재 workspace 버퍼만 보기 |
| `SPC b B` | consult-buffer-other-window | 다른 윈도우에서 버퍼 전환 |
| `SPC b d` | kill-current-buffer | 버퍼 닫기 |
| `SPC b r` | revert-buffer | 버퍼 새로고침 |

---

## 🪟 Window Management

### Leader Keys (SPC w)

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `SPC w h/j/k/l` | evil-window-left/down/up/right | 윈도우 이동 |
| `SPC w s` | evil-window-split | 수평 분할 |
| `SPC w v` | evil-window-vsplit | 수직 분할 |
| `SPC w d` | delete-window | 윈도우 닫기 |
| `SPC w o` | other-window | 다른 윈도우로 |
| `SPC w r` | evil-window-rotate-upwards | 윈도우 회전 ↻ |
| `SPC w R` | evil-window-rotate-downwards | 윈도우 회전 ↺ |

### Evil Window Commands (C-w prefix)

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `C-w h/j/k/l` | Navigate windows | 윈도우 이동 |
| `C-w =` | balance-windows | 윈도우 크기 균등 |
| `C-w _` | maximize-window | 세로 최대화 |
| `C-w \|` | maximize-window | 가로 최대화 |
| `C-w c` | delete-window | 윈도우 닫기 |
| `C-w o` | delete-other-windows | 다른 윈도우 모두 닫기 |

---

## 🏢 Workspace Management (Perspective)

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `SPC TAB 1-9` | sleepy/persp-1-9 | 워크스페이스 1-9로 전환 |
| `SPC TAB 0` | sleepy/persp-0 | 워크스페이스 0으로 전환 |
| `SPC TAB TAB` | sleepy/persp-switch-completing | 워크스페이스 선택 메뉴 |
| `SPC p p` | projectile-switch-project | 프로젝트 전환 (자동 워크스페이스) |

---

## 🎯 Git Operations (Magit)

### Leader Keys (SPC g)

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `SPC g g` | magit-status | Git 상태 ⭐ |
| `SPC g d` | git-timemachine-toggle | 파일 히스토리 보기 |
| `SPC g D` | magit-diff-buffer-file | 현재 파일 diff |
| `SPC g E` | ediff-buffers | 버퍼 diff |

### Magit Status Buffer

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `s` | magit-stage | Stage 파일/hunk |
| `u` | magit-unstage | Unstage |
| `c c` | magit-commit | Commit |
| `P p` | magit-push | Push |
| `F p` | magit-pull | Pull |
| `TAB` | magit-section-toggle | 섹션 접기/펼치기 |
| `g` | magit-refresh | 새로고침 |

---

## 💡 Useful Combinations

### Quick Navigation Pattern
```
SPC s i        → 함수 목록 보기
엔터           → 함수로 이동
C-o            → 돌아오기
```

### Search & Edit Pattern
```
SPC s y        → 커서 심볼 프로젝트 전체 검색
C-.            → E → Export to grep buffer
편집           → wgrep로 일괄 편집
C-c C-c        → 적용
```

### Multi-line Editing
```
Visual mode    → 영역 선택
C-M-j/k        → 커서 추가
편집           → 모든 줄에 동시 적용
```

### Code Navigation Loop
```
[f / ]f        → 함수 간 이동
SPC c d        → 정의로 이동
C-o            → 돌아오기
g;             → 마지막 변경 위치
```

### Smart Surround
```
ysiw"          → 단어를 "로 감싸기
cs"'           → "를 '로 변경
ds'            → ' 제거
```

---

## 🔤 Evil Motion Reference

### Basic Motions
| Keybinding | Description |
|-----------|-------------|
| `h/j/k/l` | ←/↓/↑/→ |
| `w/b` | 다음/이전 단어 시작 |
| `e` | 단어 끝 |
| `0` | 라인 시작 |
| `$` | 라인 끝 |
| `^` | 첫 non-blank 문자 |
| `gg` | 파일 시작 |
| `G` | 파일 끝 |
| `{/}` | 이전/다음 단락 |
| `%` | 매칭 괄호로 이동 |

### Search Motions
| Keybinding | Description |
|-----------|-------------|
| `f{char}` | 앞으로 char 찾기 |
| `F{char}` | 뒤로 char 찾기 |
| `t{char}` | char 직전까지 |
| `T{char}` | char 직후까지 |
| `;` | 반복 |
| `,` | 반대 방향 반복 |
| `*` | 커서 단어 앞으로 검색 |
| `#` | 커서 단어 뒤로 검색 |

---

## 📚 Help System

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `SPC h k` | describe-key | 키 설명 |
| `SPC h f` | describe-function | 함수 설명 |
| `SPC h v` | describe-variable | 변수 설명 |
| `SPC h m` | describe-mode | 모드 설명 |
| `SPC h F` | describe-face | Face 설명 |
| `SPC h K` | describe-keymap | Keymap 설명 |

---

## 🎨 Other Useful Keys

| Keybinding | Function | Description |
|-----------|----------|-------------|
| `SPC :` | eval-expression | 표현식 실행 |
| `SPC !` | shell-command | 셸 명령 실행 |
| `SPC o p` | proced | 프로세스 관리자 |
| `M-x` | execute-extended-command | 명령 실행 |
| `C-=` | text-scale-increase | 글씨 크게 |
| `C--` | text-scale-decrease | 글씨 작게 |

---

**Note:** This guide is based on the current configuration. For language-specific workflows, see:
- `python-workflow.md` - Python development
- `latex-workflow.md` - LaTeX writing
- `cheatsheet.md` - Quick reference
