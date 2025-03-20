;;; general.el --- general.el -*- no-byte-compile: t; lexical-binding: t; -*-

(defconst sleepy/leader-key "SPC")
(defconst sleepy/global-leader-key "C-SPC")


(use-package general
  :demand t
  :ensure (:wait t)
  :config
  (general-evil-setup)

  ;;; create definers
  (general-create-definer sleepy/leader-def
	:states        '(normal visual insert emacs)
    :keymaps       'override
	:prefix        "SPC"
	:global-prefix "M-SPC")

  ;;; define menus
  (sleepy/leader-def
	"c"   '(:ignore t :which-key "Code"))


  ;;; setup global keybindings
  (general-define-key
   "s-x" 'execute-extended-command
   "C-," 'duplicate-line
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease)


   ;;; setup leader keybindings
  (sleepy/leader-def
	"SPC" 'find-file
	"-" 'dired-jump
	"!" 'shell-command
	":" 'eval-expression
	"." 'repeat
    "!" 'shell-command
    "&" 'async-shell-command)

  (sleepy/leader-def
	"h"   '(:ignore t :which-key "Help")
	"h m" '(describe-mode     :which-key "desc mode")
	"h k" '(describe-key      :which-key "desc key")
	"h K" '(describe-keymap   :which-key "desc key map")
	"h f" '(describe-function :which-key "desc func")
	"h F" '(describe-Face     :which-key "desc face")
	"h v" '(describe-variable :which-key "desc variable"))

  ;; Perspective keybindings
  (sleepy/leader-def
	"TAB" '(perspective-map :wk "Perspective"))

  ;; Window keybindings
  (sleepy/leader-def
	"w"   '(:ignore t :which-key "Window")
	"w o" 'other-window
	"w d" 'delete-window
	"w r" 'evil-window-rotate-upwards
	"w R" 'evil-window-rotate-upwards
	"w e" 'evil-window-exchange
	"w h" 'evil-window-left
	"w j" 'evil-window-down
	"w k" 'evil-window-up
	"w l" 'evil-window-right
	"w s" 'evil-window-split
	"w v" 'evil-window-vsplit)


  ;; Buffer keybindings
  (sleepy/leader-def
	"b"   '(:ignore t :which-key "Buffer")
	"bi" 'ibuffer
	"bd" 'kill-current-buffer
	"br" 'revert-buffer
	"bb" 'consult-buffer
	"bB" 'consult-buffer-other-window
	"bp" 'consult-project-buffer)

  ;; Search keybindings
  (sleepy/leader-def
	"s"   '(:ignore t :which-key "Serch")
	"si" 'consult-imenu
	"sI" 'consult-imenu-multi
	"sb" 'consult-line
	"sB" 'consult-line-multi
	"sd" 'consult-ripgrep-current
	"sp" 'consult-ripgrep
	"sg" 'consult-git-grep)

  ;; Jump keybindings
  (sleepy/leader-def
	"j"   '(:ignore t :which-key "Jump")
	"ji" 'consult-outline
	"jl" 'consult-goto-line
	"jo" 'consult-outline
	"jd" 'consult-ripgrep
	"jg" 'consult-git-grep)

  ;; Project
  (sleepy/leader-def
	"p"   '(:ignore t :which-key "Project")
	"pc" 'project-compile
	"pp" 'project-recompile
	"p-" 'projectile-dired
	"p_" 'projectile-dired-other-window
	"p!" 'projectile-run-shell-command-in-root
	"p%" 'projectile-replace-regexp
	"p&" 'projectile-run-async-shell-command-in-root
	"p-" 'projectile-dired
	"pf" 'projectile-find-file
	"pe" 'projectile-edit-dir-locals
	"pI" 'projectile-invalidate-cache
	"pk" 'projectile-kill-buffers
	"pR" 'projectile-replace
	"pS" 'projectile-save-project-buffers
	"pv" 'projectile-vc)

  (sleepy/leader-def
	"uy" 'consult-yank-pop)

  (sleepy/leader-def
	"g"   '(:ignore t :which-key "Git")
	"gg" 'magit-status
	"gt" 'git-timemachine)

  ;; File keybindings
  (sleepy/leader-def
	"f"   '(:ignore t :which-key "File")
	"ff" 'find-file
	"fF" 'find-file-other-window
	"fr" 'consult-recent-file)

  (general-evil-define-key 'normal prog-mode-map
	"gd" #'xref-find-definitions
	"gr" #'xref-find-references))
