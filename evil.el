;;; evil.el --- lean evil setup using package defaults (no :general) -*- lexical-binding: t; -*-

;; ==============================================================================
;; EVIL COMMANDS REFERENCE GUIDE
;; ==============================================================================
;;
;; ---- G PREFIX COMMANDS ------------------------------------------------------
;; gf        - Go to file under cursor (find-file-at-point)
;; gd        - Go to definition (xref-find-definitions)
;; gx/gX     - Exchange text regions (evil-exchange - installed)
;; gc        - Comment operator (gcc for line, gc{motion} for region)
;; gu{motion}- Convert to lowercase (guiw = lowercase word, guap = lowercase paragraph)
;; gU{motion}- Convert to uppercase (gUiw = uppercase word)
;; g~{motion}- Toggle case
;; gq{motion}- Format/wrap text (great for comments)
;; g;/g,     - Go to previous/next change location in change list
;; gv        - Reselect last visual selection
;; gi        - Go to last insert position and enter insert mode
;; gJ        - Join lines without spaces
;;
;; ---- Z PREFIX COMMANDS (FOLDING & SCROLLING) --------------------------------
;; zz        - Center current line on screen
;; zt        - Current line to top of screen
;; zb        - Current line to bottom of screen
;; za        - Toggle fold at cursor
;; zo/zc     - Open/close fold
;; zM/zR     - Close/open all folds
;; zm/zr     - Fold more/fold less (increase/decrease fold level)
;; zf{motion}- Create a fold
;;
;; ---- NAVIGATION PAIRS [ AND ] -----------------------------------------------
;; [b/]b     - Previous/next buffer
;; [f/]f     - Previous/next function (configured with tree-sitter)
;; [g/]g     - Previous/next class (configured with tree-sitter)
;; [[/]]     - Previous/next section/function
;; [{/]}     - Previous/next unmatched brace
;; [(/])     - Previous/next unmatched parenthesis
;; [m/]m     - Previous/next method start
;; [M/]M     - Previous/next method end
;;
;; ---- MARKS AND REGISTERS ----------------------------------------------------
;; m{a-z}    - Set local mark (buffer-specific)
;; m{A-Z}    - Set global mark (across buffers)
;; '{mark}   - Jump to beginning of marked line
;; `{mark}   - Jump to exact mark position
;; ''        - Jump to line of last jump
;; ``        - Jump to exact position before last jump
;; `.        - Jump to last change
;; `^        - Jump to last insert position
;; :marks    - List all marks
;;
;; "{reg}y   - Yank to register (a-z for named, 0 for yank, " for default)
;; "{reg}p   - Paste from register
;; "0p       - Paste from yank register (not affected by delete/change)
;; "+y/"+p   - System clipboard yank/paste
;; "*y/"*p   - X11 primary selection (Linux)
;; :reg      - Show all registers
;;
;; ---- POWERFUL TEXT OBJECTS (USE WITH d/c/y/v) -------------------------------
;; iw/aw     - Inner/around word
;; iW/aW     - Inner/around WORD (includes special chars)
;; is/as     - Inner/around sentence
;; ip/ap     - Inner/around paragraph
;; i"/a"     - Inner/around double quotes
;; i'/a'     - Inner/around single quotes
;; i`/a`     - Inner/around backticks
;; ib/ab     - Inner/around () parentheses (same as i(/a()
;; iB/aB     - Inner/around {} braces (same as i{/a{)
;; it/at     - Inner/around HTML/XML tags
;; ia/aa     - Inner/around argument (evil-args - installed)
;; il/al     - Inner/around line (evil-textobj-line - installed)
;; if/af     - Inner/around function (tree-sitter - installed)
;; ig/ag     - Inner/around class (tree-sitter - installed)
;;
;; ---- WINDOW COMMANDS (C-w PREFIX) -------------------------------------------
;; C-w v     - Vertical split
;; C-w s     - Horizontal split
;; C-w w     - Cycle windows
;; C-w h/j/k/l - Navigate windows
;; C-w H/J/K/L - Move window to far left/bottom/top/right
;; C-w =     - Balance window sizes
;; C-w _     - Maximize window height
;; C-w |     - Maximize window width
;; C-w r/R   - Rotate windows forward/backward
;; C-w x     - Exchange windows
;; C-w c     - Close window
;; C-w o     - Close all other windows
;; C-w T     - Move window to new tab
;;
;; ---- USEFUL OPERATORS -------------------------------------------------------
;; !{motion} - Filter through external command
;; ={motion} - Auto-indent
;; gw{motion}- Format text without moving cursor
;; >{motion} - Indent right
;; <{motion} - Indent left
;;
;; ---- VISUAL MODE SPECIFIC ---------------------------------------------------
;; o         - Jump to other end of selection
;; O         - Jump to other corner (block selection)
;; gv        - Reselect last visual selection
;; V         - Line-wise visual mode
;; C-v       - Block visual mode
;; I/A       - Insert at beginning/end of each line (block mode)
;; r{char}   - Replace selection with character
;; J         - Join selected lines
;; u/U       - Convert selection to lower/uppercase
;; */##      - Search forward/backward for selected text (evil-visualstar - installed)
;;
;; ---- SPECIAL COMMANDS -------------------------------------------------------
;; .         - Repeat last change
;; @{reg}    - Execute macro from register (@@ to repeat)
;; q{reg}    - Record macro to register (q to stop)
;; Q         - Ex mode (rarely used)
;; &         - Repeat last substitution
;; g&        - Repeat last substitution globally
;; C-a/C-x   - Increment/decrement number under cursor
;; g C-a/g C-x - Increment/decrement in visual selection (sequential)
;;
;; ---- CONFIGURED EXTRAS ------------------------------------------------------
;; L/H       - Next/previous argument (evil-args)
;; K         - Jump out of arguments (evil-args)
;; gx/gX     - Exchange operator (evil-exchange)
;; gc        - Comment operator
;; s/S       - Surround in visual mode (evil-surround)
;; ys{motion}- Add surround (ysiwb = surround inner word with parens)
;; cs{old}{new} - Change surround (cs"' = change " to ')
;; ds{char}  - Delete surround
;; M-d       - Start multiple cursors (evil-multiedit)
;;
;; ---- USEFUL COMBINATIONS ----------------------------------------------------
;; ciw       - Change inner word
;; ci"       - Change inside quotes
;; da)       - Delete around parentheses
;; yi{       - Yank inside braces
;; vap       - Select around paragraph
;; >i}       - Indent inside braces
;; gcap      - Comment a paragraph
;; gUiw      - Uppercase inner word
;; ysiwb     - Surround word with parentheses
;; ysiw"     - Surround word with quotes
;; va"yi"p   - Select around quotes, yank inside quotes, paste
;;
;; ==============================================================================

;; ---- Undo stack -------------------------------------------------------------
(use-package undo-fu :ensure t)
(use-package undo-fu-session
  :ensure t
  :init (undo-fu-session-global-mode 1))

;; ---- Evil core --------------------------------------------------------------
(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-fu
        evil-want-Y-yank-to-eol t
        evil-respect-visual-line-mode t
        evil-want-fine-undo t)
  :config
  (evil-mode 1)

  ;; Search module
  (evil-select-search-module 'evil-search-module 'isearch)

  ;; Mark as jump commands
  (evil-set-command-property 'xref-find-definitions :jump t)
  (evil-set-command-property 'xref-find-references :jump t)

  ;; Some Emacs-style keys (preserve original settings)
  (define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-f") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "C-A") 'evil-beginning-of-visual-line)
  (define-key evil-normal-state-map (kbd "C-E") 'evil-end-of-visual-line)
  (define-key evil-motion-state-map "_" 'evil-end-of-line)
  (define-key evil-motion-state-map "0" 'evil-beginning-of-line)

  ;; Evil state exceptions
  (dolist (mode '(custom-mode eshell-mode shell-mode term-mode vterm-mode eat-mode
                  elpaca-ui-mode calc-mode inferior-python-mode wdired-mode
                  log-edit-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  (evil-set-initial-state 'debugger-mode 'motion)
  (evil-set-initial-state 'pdf-view-mode 'motion)
  (evil-set-initial-state 'git-commit-mode 'insert)

  ;; Comment toggle operator: gc
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for region."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment)

  ;; Go to file under cursor with gf (standard Vim behavior)
  (evil-define-key 'normal 'global (kbd "gf") 'find-file-at-point))

;; ---- Evil collection (use package default evil bindings) -------------------
(use-package evil-collection
  :after evil
  :ensure t
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

;; ---- Surround ---------------------------------------------------------------
(use-package evil-surround
  :ensure t
  :after (evil general)
  :config
  (global-evil-surround-mode 1)

  (general-define-key
   :states 'visual
   "s" 'evil-surround-region
   "S" 'evil-Surround-region)
  (general-define-key
   :states 'operator
   "s" 'evil-surround-edit))

;; ---- Other text objects/tools -----------------------------------------------
(use-package evil-textobj-line :ensure t :after evil)

(use-package evil-args
  :ensure t
  :after evil
  :config
  ;; text object
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  ;; movement/jump
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(use-package evil-exchange
  :ensure t
  :init
  (setq evil-exchange-key (kbd "gx")
        evil-exchange-cancel-key (kbd "gX"))
  :config
  (evil-exchange-install))

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-visualstar
  :ensure t
  :after evil
  :commands global-evil-visualstar-mode
  :hook (after-init . global-evil-visualstar-mode))

;; ---- vdiff (optional) -------------------------------------------------------
(use-package vdiff
  :ensure t
  :commands (vdiff-buffers vdiff-buffers3 vdiff-quit vdiff-files vdiff-files3)
  :custom
  (vdiff-auto-refine t)
  (vdiff-only-highlight-refinements t))

;; ---- Motion map cleanup -----------------------------------------------------
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

;; ---- Better-jumper (prevent C-i conflict) -----------------------------------
(use-package better-jumper
  :ensure t
  :init (better-jumper-mode 1)
  :config
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
    ;; <C-i> can conflict with TAB → use alternative key
    (define-key evil-motion-state-map (kbd "M-]") 'better-jumper-jump-forward)))

;; ---- Tree-sitter text objects (optional) ------------------------------------
(use-package evil-textobj-tree-sitter
  :ensure t
  :after evil
  :config

  ;; function
  (define-key evil-outer-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-normal-state-map (kbd "]f")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  (define-key evil-normal-state-map (kbd "[f")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  (define-key evil-normal-state-map (kbd "]F")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[F")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))

  ;; class
  (define-key evil-outer-text-objects-map "g"
    (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "g"
    (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-normal-state-map (kbd "]g")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer")))
  (define-key evil-normal-state-map (kbd "[g")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t)))
  (define-key evil-normal-state-map (kbd "]G")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[G")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t t))))
