(defconst sleepy/leader-key "SPC")
(defconst sleepy/global-leader-key "C-SPC")


(elpaca-use-package general
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

  (general-define-key
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease)


  ;;; setup leader keybindings
  (sleepy/leader-keys
    "SPC" 'find-file
    "-" 'dired
    "!" 'shell-command
    ":" 'eval-expression
    "." 'repeat)

  (sleepy/file-keys
   "f" '(find-file              :which-key "find-file")
   "F" '(find-file-other-window :which-key "find-file-other-window"))

  (sleepy/help-keys
   "m" '(describe-mode      :which-key "desc mode")
   "k" '(describe-key      :which-key "desc key")
   "f" '(describe-function :which-key "desc func")
   "F" '(describe-Face     :which-key "desc face")
   "v" '(describe-variable :which-key "desc variable"))

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
