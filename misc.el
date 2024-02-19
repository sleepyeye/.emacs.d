(use-package explain-pause-mode
  :ensure (explain-pause-mode :host github :repo "lastquestion/explain-pause-mode")
  :defer t)


(use-package simple
  :ensure nil
  :general
  (+general-global-toggle
    "f" 'auto-fill-mode)
  :custom
  (eval-expression-debug-on-error nil)
  (fill-column 80 "Wrap at 80 columns."))

(use-package autorevert
  :ensure nil
  :defer 2
  :custom
  (auto-revert-interval 0.01 "Instantaneously revert")
  :config
  (global-auto-revert-mode t))

(use-package default-text-scale
  :commands ( default-text-scale-increase
              default-text-scale-decrease
              default-text-scale-reset
              default-text-scale-increment))


(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))


(use-package vterm
  :ensure t
  :config
  (setq vterm-shell (executable-find "fish")
        vterm-max-scrollback 10000))


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


(use-package jinx
  :after (latex auctex)
  :demand t
  :bind ([remap ispell-word] . jinx-correct)
  :hook (emacs-startup . global-jinx-mode)
  :config
  (setq jinx-languages "en")
  (set-face-attribute 'jinx-misspelled nil :underline '(:color "#ffcc00" :style wave)))
