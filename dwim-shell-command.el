(use-package dwim-shell-command
  :defer 3
  :if (memq window-system '(mac ns))
  :bind (([remap shell-command] . dwim-shell-command)
		 :map dired-mode-map
		 ([remap dired-do-async-shell-command] . dwim-shell-command)
		 ([remap dired-do-shell-command] . dwim-shell-command)
		 ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  (require 'dwim-shell-commands))
