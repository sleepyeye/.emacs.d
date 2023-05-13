(use-package drag-stuff
  :demand t
  :diminish drag-stuff-mode
  :config
  (drag-stuff-global-mode t)
  (general-define-key
   :keymaps 'override
   "M-j" #'drag-stuff-down
   "M-k" #'drag-stuff-up
   "M-h" #'drag-stuff-left
   "M-l" #'drag-stuff-right))

(use-package ialign)
