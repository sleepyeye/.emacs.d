(defconst sleepy/leader-key "SPC")
(defconst sleepy/global-leader-key "C-SPC")


(use-package general
  :demand t
  :after evil
  :config

  (general-override-mode)
  (general-auto-unbind-keys)
  (general-evil-setup)

  ;;; define prefix map
  (general-define-key
   :keymaps       'override
   :states        '(insert normal hybrid motion visual operator emacs)
   :prefix-map    '+prefix-map
   :prefix        "SPC"
   :global-prefix "S-SPC")

  ;;; create definers
  (general-create-definer sleepy/leader-keys
    :wk-full-keys nil
    :keymaps      '+prefix-map

    ;; define menus
    "h"   '(:ignore t :which-key "Help")
    "c"   '(:ignore t :which-key "Code")
    "p"   '(:ignore t :which-key "Project")
    "s"   '(:ignore t :which-key "Search")
    "j"   '(:ignore t :which-key "Jump")
    "b"   '(:ignore t :which-key "Buffer")
    "f"   '(:ignore t :which-key "File")
    "g"   '(:ignore t :which-key "Utils")
    "w"   '(:ignore t :which-key "Window")
    "TAB" '(:ignore t :which-key "Workspace"))


  ;;; defines key definers for menus
  (general-create-definer sleepy/help-keys
    :wrapping sleepy/leader-keys
    :infix     "h"
    :wk-full-keys: nil)

  (general-create-definer sleepy/code-keys
    :wrapping sleepy/leader-keys
    :infix     "c"
    :wk-full-keys: nil)

  (general-create-definer sleepy/project-keys
    :wrapping sleepy/leader-keys
    :infix     "p"
    :wk-full-keys: nil)

  (general-create-definer sleepy/search-keys
    :wrapping sleepy/leader-keys
    :infix     "s"
    :wk-full-keys: nil)

  (general-create-definer sleepy/jump-keys
    :wrapping sleepy/leader-keys
    :infix     "j"
    :wk-full-keys: nil)

  (general-create-definer sleepy/buffer-keys
    :wrapping sleepy/leader-keys
    :infix     "b"
    :wk-full-keys: nil)

  (general-create-definer sleepy/file-keys
    :wrapping sleepy/leader-keys
    :infix     "f"
    :wk-full-keys: nil)

  (general-create-definer sleepy/window-keys
    :wrapping sleepy/leader-keys
    :infix     "w"
    :wk-full-keys: nil)

  (general-create-definer sleepy/workspace-keys
    :wrapping sleepy/leader-keys
    :infix     "TAB"
    :wk-full-keys: nil)

  (general-create-definer sleepy/util-keys
    :wrapping sleepy/leader-keys
    :infix     "g"
    :wk-full-keys: nil)

  (general-create-definer sleepy/python-keys
    :states '(normal visual motion)
    :keymaps 'python-mode-map
    :prefix "SPC m"
    :non-normal-prefix "S-SPC")



  (general-define-key
   "s-x" 'execute-extended-command
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease)


  ;;; setup leader keybindings
  (sleepy/leader-keys
    "SPC" 'find-file
    "-" 'dired-jump
    "!" 'shell-command
    ":" 'eval-expression
    "." 'repeat)

  (sleepy/file-keys
   "f" 'find-file             
   "F" 'find-file-other-window)

  (sleepy/help-keys
   "m" '(describe-mode      :which-key "desc mode")
   "k" '(describe-key      :which-key "desc key")
   "K" '(describe-keymap      :which-key "desc key map")
   "f" '(describe-function :which-key "desc func")
   "F" '(describe-Face     :which-key "desc face")
   "v" '(describe-variable :which-key "desc variable"))

  (sleepy/workspace-keys
    "1" '(:ignore t :which-key "workspace 1")
    "2" '(:ignore t :which-key "workspace 2")
    "3" '(:ignore t :which-key "workspace 3")
    "4" '(:ignore t :which-key "workspace 4")
    "5" '(:ignore t :which-key "workspace 5")
    "6" '(:ignore t :which-key "workspace 6")
    "7" '(:ignore t :which-key "workspace 7")
    "8" '(:ignore t :which-key "workspace 8")
    "9" '(:ignore t :which-key "workspace 9"))

  (sleepy/window-keys
   "o" 'other-window
   "d" 'delete-window
   "r" 'evil-window-rotate-upwards
   "R" 'evil-window-rotate-upwards
   "e" 'evil-window-exchange
   "h" 'evil-window-left
   "j" 'evil-window-down
   "k" 'evil-window-up
   "l" 'evil-window-right
   "s" 'evil-window-split
   "v" 'evil-window-vsplit))
(elpaca-wait)
