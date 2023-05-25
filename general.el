(defconst sleepy/leader-key "SPC")
(defconst sleepy/global-leader-key "C-SPC")


(use-package general
  :demand t
  :after evil
  :config

  (general-override-mode)
  (general-auto-unbind-keys)
  (general-evil-setup)

	;;; create definers
  (general-create-definer sleepy/leader-def
	:wk-full-keys nil
	:keymaps       'override
	:states        '(insert normal hybrid motion visual operator emacs)
	:prefix        "SPC"
	:global-prefix "S-SPC"

	;; define menus
	"a"   '(:ignore t :which-key "AI")
	"h"   '(:ignore t :which-key "Help")
	"c"   '(:ignore t :which-key "Code")
	"p"   '(:ignore t :which-key "Project")
	"s"   '(:ignore t :which-key "Search")
	"j"   '(:ignore t :which-key "Jump")
	"b"   '(:ignore t :which-key "Buffer")
	"f"   '(:ignore t :which-key "File")
	"g"   '(:ignore t :which-key "Utils")
	"w"   '(:ignore t :which-key "Window")
	"r"   '(:ignore t :which-key "Org-roam")
	"TAB" '(:ignore t :which-key "Workspace"))


  (general-define-key
   "s-x" 'execute-extended-command
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease)


	;;; setup leader keybindings
  (sleepy/leader-def
	"SPC" 'find-file
	"-" 'dired-jump
	"!" 'shell-command
	":" 'eval-expression
	"." 'repeat)

  (sleepy/leader-def
	"ff" 'find-file
	"fF" 'find-file-other-window)

  (sleepy/leader-def
	"hm" '(describe-mode     :which-key "desc mode")
	"hk" '(describe-key      :which-key "desc key")
	"hK" '(describe-keymap   :which-key "desc key map")
	"hf" '(describe-function :which-key "desc func")
	"hF" '(describe-Face     :which-key "desc face")
	"hv" '(describe-variable :which-key "desc variable"))

  (sleepy/leader-def
	"TAB 1" '(:ignore t :which-key "workspace 1")
	"TAB 2" '(:ignore t :which-key "workspace 2")
	"TAB 3" '(:ignore t :which-key "workspace 3")
	"TAB 4" '(:ignore t :which-key "workspace 4")
	"TAB 5" '(:ignore t :which-key "workspace 5")
	"TAB 6" '(:ignore t :which-key "workspace 6")
	"TAB 7" '(:ignore t :which-key "workspace 7")
	"TAB 8" '(:ignore t :which-key "workspace 8")
	"TAB 9" '(:ignore t :which-key "workspace 9"))

  (sleepy/leader-def
	"wo" 'other-window
	"wd" 'delete-window
	"wr" 'evil-window-rotate-upwards
	"wR" 'evil-window-rotate-upwards
	"we" 'evil-window-exchange
	"wh" 'evil-window-left
	"wj" 'evil-window-down
	"wk" 'evil-window-up
	"wl" 'evil-window-right
	"ws" 'evil-window-split
	"wv" 'evil-window-vsplit)

  (general-evil-define-key 'normal prog-mode-map
	"gd" #'xref-find-definitions
	"gr" #'xref-find-references)

  )
(elpaca-wait)
