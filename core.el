;;; core.el --- general, evil, and editing config -*- lexical-binding: t; -*-

(defconst sleepy/leader-key "SPC")
(defconst sleepy/global-leader-key "M-SPC")

;; Helper function for renaming visited file
(defun sleepy/rename-visited-file (new-name)
  "Rename the file being visited to NEW-NAME.
The buffer name is also updated to match the new file name.

This function performs comprehensive validation before renaming:
- Ensures buffer is visiting a file
- Validates new filename is not empty
- Checks target directory exists and is writable
- Confirms overwrite if target file exists
- Handles errors gracefully

NEW-NAME should be an absolute or relative file path."
  (interactive "FNew name: ")
  (let ((old-name (buffer-file-name)))
    ;; Validate preconditions
    (unless old-name
      (user-error "Buffer is not visiting a file"))
    (when (string-empty-p new-name)
      (user-error "New filename cannot be empty"))
    (when (file-directory-p new-name)
      (user-error "Target is a directory, not a file"))

    ;; Ensure target directory exists and is writable
    (let ((target-dir (file-name-directory (expand-file-name new-name))))
      (unless (file-directory-p target-dir)
        (user-error "Target directory does not exist: %s" target-dir))
      (unless (file-writable-p target-dir)
        (user-error "Target directory is not writable: %s" target-dir)))

    ;; Confirm overwrite if file exists
    (when (file-exists-p new-name)
      (unless (y-or-n-p (format "File %s already exists. Overwrite? " new-name))
        (user-error "Rename cancelled")))

    ;; Perform rename with error handling
    (condition-case err
        (progn
          (rename-file old-name new-name 1)
          (set-visited-file-name new-name)
          (rename-buffer (file-name-nondirectory new-name))
          (set-buffer-modified-p nil)
          (message "File renamed to %s" new-name))
      (file-error
       (user-error "Failed to rename file: %s" (error-message-string err))))))

(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-evil-setup)

  (general-create-definer sleepy/leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix sleepy/leader-key
    :global-prefix sleepy/global-leader-key)

  ;; Minimal global keys
  (general-define-key
   "M-x" 'execute-extended-command
   "s-x" 'execute-extended-command
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease)

  ;; Leader menu
  (sleepy/leader-def
    "SPC" '(find-file :which-key "find file")
    "-"   '(dired-jump :which-key "dired here")
    ":"   '(eval-expression :which-key "eval")
    "!"   '(shell-command :which-key "shell cmd")
    "&"   '(async-shell-command :which-key "async shell"))

  (sleepy/leader-def
	"f"   '(:ignore t :which-key "File")
	"ff"  'find-file
	"fp"  'projectile-find-file
	"fg"  'consult-ripgrep
	"fl"  'consult-line
	"fr"  'consult-recent-file
	"fP"  'projectile-recentf
	"fd"  'dired-jump
	"fs"  'save-buffer
	"fu"  'revert-buffer
	"fR"  'sleepy/rename-visited-file
	"fx"  'delete-file)


  (sleepy/leader-def
    "h"   '(:ignore t :which-key "Help")
    "h m" '(describe-mode     :which-key "mode")
    "h k" '(describe-key      :which-key "key")
    "h K" '(describe-keymap   :which-key "keymap")
    "h f" '(describe-function :which-key "func")
    "h F" '(describe-face     :which-key "face")
    "h v" '(describe-variable :which-key "var"))

  (sleepy/leader-def
    "o"   '(:ignore t :which-key "Open")
    "o p" '(proced :which-key "process manager"))

  (sleepy/leader-def
    "w"   '(:ignore t :which-key "Window")
    "w o" '(other-window :which-key "other")
    "w d" '(delete-window :which-key "delete")
    "w s" '(evil-window-split :which-key "split-h")
    "w v" '(evil-window-vsplit :which-key "split-v")
    "w r" '(evil-window-rotate-upwards :which-key "rotate ↻")
    "w R" '(evil-window-rotate-downwards :which-key "rotate ↺")
    "w h" '(evil-window-left :which-key "←")
    "w j" '(evil-window-down :which-key "↓")
    "w k" '(evil-window-up   :which-key "↑")
    "w l" '(evil-window-right :which-key "→"))

  (sleepy/leader-def
    "b"   '(:ignore t :which-key "Buffer")
    "bb" '(consult-buffer :which-key "switch")
    "bB" '(consult-buffer-other-window :which-key "switch other")
    "bd" '(kill-current-buffer :which-key "kill")
    "br" '(revert-buffer :which-key "revert"))

  (sleepy/leader-def
    "B"   '(:ignore t :which-key "Bookmark")
    "Bb" '(consult-bookmark :which-key "jump")
    "Bs" '(bookmark-set :which-key "set")
    "Bd" '(bookmark-delete :which-key "delete")
    "Bl" '(bookmark-bmenu-list :which-key "list")
    "BR" '(bookmark-rename :which-key "rename"))

  (sleepy/leader-def
    "s"   '(:ignore t :which-key "Search")
    "sb" '(consult-line :which-key "in buffer")
    "sB" '(consult-line-multi :which-key "multi buf")
    "sp" '(consult-ripgrep :which-key "ripgrep proj")
    "sd" '(consult-ripgrep-current :which-key "ripgrep here")
    "si" '(consult-imenu :which-key "imenu")
    "sI" '(consult-imenu-multi :which-key "imenu*"))

  (sleepy/leader-def
    "g"   '(:ignore t :which-key "Git")
    "gg" '(magit-status :which-key "status"))

  (sleepy/leader-def
    "p" '(projectile-command-map :which-key "Project"))

  )

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
;; [f/]f     - Previous/next function (tree-sitter)
;; [g/]g     - Previous/next class (tree-sitter)
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
  :hook (elpaca-after-init . undo-fu-session-global-mode))

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
  (general-define-key
   :states 'normal
   :keymaps 'override
   "C-b" 'evil-scroll-up
   "C-f" 'evil-scroll-down
   "C-n" 'evil-next-line
   "C-p" 'evil-previous-line
   "C-a" 'evil-beginning-of-line
   "C-e" 'evil-end-of-line
   "C-A" 'evil-beginning-of-visual-line
   "C-E" 'evil-end-of-visual-line)

  (general-define-key
   :states 'motion
   "_" 'evil-end-of-line
   "0" 'evil-beginning-of-line)

  ;; Evil state exceptions
  (dolist (mode '(custom-mode eshell-mode shell-mode term-mode vterm-mode
							  elpaca-ui-mode calc-mode inferior-python-mode wdired-mode
							  log-edit-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  (evil-set-initial-state 'debugger-mode 'motion)
  (evil-set-initial-state 'pdf-view-mode 'motion)
  (evil-set-initial-state 'image-mode 'motion)
  (evil-set-initial-state 'git-commit-mode 'insert)

  ;; Comment toggle operator: gc
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for region."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))

  ;; gc and gf bindings
  (general-define-key
   :states 'normal
   :keymaps 'global
   "gc" 'my-evil-comment-or-uncomment
   "gf" 'find-file-at-point))

;; ---- Evil collection (use package default evil bindings) -------------------
(use-package evil-collection
  :after evil
  :ensure t
  :demand t
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

;; ---- Surround ---------------------------------------------------------------
(use-package evil-surround
  :ensure t
  :demand t
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
(use-package evil-textobj-line
  :ensure t
  :demand t
  :after evil
  :commands (evil-a-line evil-inner-line))

(use-package evil-args
  :ensure t
  :demand t
  :after (evil general)
  :config
  ;; text objects
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "a" 'evil-inner-arg)
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "a" 'evil-outer-arg)
  ;; movement/jump
  (general-define-key
   :states '(normal motion)
   "L" 'evil-forward-arg
   "H" 'evil-backward-arg)
  (general-define-key
   :states 'normal
   "K" 'evil-jump-out-args))

(use-package evil-exchange
  :ensure t
  :demand t
  :init
  (setq evil-exchange-key (kbd "gx")
        evil-exchange-cancel-key (kbd "gX"))
  :config
  (evil-exchange-install))

(use-package evil-multiedit
  :ensure t
  :demand t
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-visualstar
  :ensure t
  :demand t
  :after evil
  :config
  (global-evil-visualstar-mode 1))

;; ---- vdiff (optional) -------------------------------------------------------
(use-package vdiff
  :ensure t
  :commands (vdiff-buffers vdiff-buffers3 vdiff-quit vdiff-files vdiff-files3)
  :custom
  (vdiff-auto-refine t)
  (vdiff-only-highlight-refinements t))

;; ---- Motion map cleanup -----------------------------------------------------
(with-eval-after-load 'evil-maps
  (general-define-key
   :states 'motion
   "SPC" nil
   "RET" nil
   "TAB" nil))

;; ---- Better-jumper (prevent C-i conflict) -----------------------------------
(use-package better-jumper
  :ensure t
  :after general
  :config
  (better-jumper-mode 1)
  (with-eval-after-load 'evil-maps
    (general-define-key
     :states 'motion
     "C-o" 'better-jumper-jump-backward
     ;; <C-i> can conflict with TAB - use alternative key
     "M-]" 'better-jumper-jump-forward)))

;; ---- Tree-sitter text objects (optional) ------------------------------------
(use-package evil-textobj-tree-sitter
  :ensure t
  :demand t
  :after (evil general)
  :config
  ;; function text objects
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; class text objects
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "g" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "g" (evil-textobj-tree-sitter-get-textobj "class.inner"))

  ;; function/class navigation
  (general-define-key
   :states 'normal
   "]f" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer"))
   "[f" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t))
   "]F" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t))
   "[F" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))
   "]g" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer"))
   "[g" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t))
   "]G" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t))
   "[G" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t t))))

(use-package evil-numbers
  :ensure t
  :after (evil general)
  :config
  (general-define-key
   :states 'visual
   "g C-a" 'evil-numbers/inc-at-pt
   "g C-A" 'evil-numbers/dec-at-pt))

;; Smart parentheses management
(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (text-mode . smartparens-mode)
         (LaTeX-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  ;; Evil integration
  (with-eval-after-load 'evil
    ;; Use smartparens for text objects
    (setq sp-navigate-consider-symbols nil)
    ;; Better Evil integration
    (setq sp-autoskip-closing-pair 'always
          sp-hybrid-kill-entire-symbol nil))
  ;; Don't insert space before delimiters in some modes
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  ;; Python-specific
  (sp-local-pair 'python-mode "'" nil :unless '(sp-point-after-word-p))
  ;; LaTeX-specific
  (sp-local-pair 'LaTeX-mode "$" "$")
  (sp-local-pair 'LaTeX-mode "\\[" "\\]")
  :diminish smartparens-mode)

;; iedit: multiple identical region editing (loaded on demand)
(use-package iedit
  :ensure t
  :commands (iedit-mode iedit-mode-toggle-on-function))

;; evil-mc: multiple cursors for Evil (manual cursor placement)
(use-package evil-mc
  :ensure t
  :after (evil general)
  :config
  ;; global-evil-mc-mode disabled - post-command-hook overhead on every keystroke
  ;; Enable locally with gm keybindings when needed
  ;; (global-evil-mc-mode 1)

  ;; Evil style: gm prefix (multiple cursors - consistent with other packages)
  (general-define-key
   :states 'normal
   "gmm" 'evil-mc-make-all-cursors
   "gmu" 'evil-mc-undo-all-cursors
   "gmn" 'evil-mc-make-and-goto-next-match
   "gmp" 'evil-mc-make-and-goto-prev-match
   "gmN" 'evil-mc-skip-and-goto-next-match
   "gmP" 'evil-mc-skip-and-goto-prev-match
   "gmq" 'evil-mc-pause-cursors
   "gmr" 'evil-mc-resume-cursors)

  ;; Line-wise cursor addition (normal & visual)
  (general-define-key
   :states '(normal visual)
   "C-M-j" 'evil-mc-make-cursor-move-next-line
   "C-M-k" 'evil-mc-make-cursor-move-prev-line)

  ;; Emacs style: C-c m prefix (global)
  (general-define-key
   "C-c m j" 'evil-mc-make-cursor-move-next-line
   "C-c m k" 'evil-mc-make-cursor-move-prev-line
   "C-c m n" 'evil-mc-make-and-goto-next-match
   "C-c m p" 'evil-mc-make-and-goto-prev-match
   "C-c m m" 'evil-mc-make-all-cursors
   "C-c m u" 'evil-mc-undo-all-cursors
   "C-c m q" 'evil-mc-pause-cursors
   "C-c m r" 'evil-mc-resume-cursors))

(use-package ialign
  :ensure t)

(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode wgrep-finish-edit)
  :config
  (setq wgrep-change-readonly-file t
        wgrep-auto-save-buffer t))

;; EditorConfig support for consistent coding styles across editors
(use-package editorconfig
  :ensure t
  :hook (elpaca-after-init . editorconfig-mode)
  :config
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :diminish editorconfig-mode)

(use-package expand-region
  :ensure t
  :commands (er/expand-region er/contract-region)
  :bind (("M-=" . er/expand-region)
         ("M-+" . er/expand-region)
         ("M--" . er/contract-region)))

;; Snippet expansion system
(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)
         (LaTeX-mode . yas-minor-mode))
  :init
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-reload-all)
  ;; Use TAB only when at word end/beginning for better completion integration
  (setq yas-triggers-in-field t
        yas-wrap-around-region t
        yas-verbosity 1)
  ;; Keybindings
  (with-eval-after-load 'general
    (when (fboundp 'sleepy/leader-def)
      (sleepy/leader-def
        "i s" '(yas-insert-snippet :which-key "insert snippet")
        "i n" '(yas-new-snippet :which-key "new snippet")
        "i v" '(yas-visit-snippet-file :which-key "visit snippet")))))

;; Collection of snippets for many languages
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Yasnippet completion-at-point backend for corfu integration
;; NOTE: :demand t ensures this loads immediately after yasnippet
;; so it's available when completion.el capf hooks run
(use-package yasnippet-capf
  :ensure t
  :demand t
  :after yasnippet)

;;; core.el ends here
