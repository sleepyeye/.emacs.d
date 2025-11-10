;;; general.el --- leader bindings (lean, no override of package evil bindings) -*- lexical-binding: t; -*-

(defconst sleepy/leader-key "SPC")
(defconst sleepy/global-leader-key "M-SPC")

;; Helper function for renaming visited file
(defun rename-visited-file (new-name)
  "Rename the file being visited to NEW-NAME.
The buffer name is also updated to match the new file name."
  (interactive "FNew name: ")
  (let ((old-name (buffer-file-name)))
    (unless old-name
      (error "Buffer is not visiting a file"))
    (when (file-exists-p new-name)
      (unless (y-or-n-p (format "File %s already exists. Overwrite? " new-name))
        (user-error "Rename cancelled")))
    (rename-file old-name new-name 1)
    (set-visited-file-name new-name)
    (rename-buffer (file-name-nondirectory new-name))
    (set-buffer-modified-p nil)
    (message "File renamed to %s" new-name)))

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

  ;; 최소 전역 키
  (general-define-key
   "M-x" 'execute-extended-command
   "s-x" 'execute-extended-command
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease)

  ;; Leader 메뉴
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
	"fR"  'rename-visited-file
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

  ;; Registers - Enhanced system with DWIM commands + completion
  (sleepy/leader-def
    "r"   '(:ignore t :which-key "Registers")
    "r a" '(sleepy/register-add-dwim :which-key "add DWIM")
    "r u" '(sleepy/register-use-dwim :which-key "use DWIM")
    "r j" '(sleepy/register-jump :which-key "jump only")
    "r i" '(sleepy/register-insert :which-key "insert only")
    "r f" '(sleepy/register-save-file-position :which-key "file+pos")
    "r l" '(consult-register :which-key "list all")
    "r s" '(consult-register-store :which-key "store (consult)")
    "r L" '(consult-register-load :which-key "load (consult)"))

  ;; AI/Claude writing assistance
  (sleepy/leader-def
    "a"   '(:ignore t :which-key "AI/Claude")
    "ac" '(claude-code-ide-menu :which-key "claude menu")
    "aw" '(sleepy/claude-write :which-key "write")
    "ar" '(sleepy/claude-rewrite-region :which-key "rewrite region")
    "ap" '(sleepy/claude-proofread-region :which-key "proofread")
    "ae" '(sleepy/claude-explain :which-key "explain")
    "as" '(sleepy/claude-summarize :which-key "summarize")
    "ai" '(sleepy/claude-improve-writing :which-key "improve writing")
    "am" '(sleepy/claude-email :which-key "compose email"))
)
