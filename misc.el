(use-package default-text-scale
  :defer t
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
  :hook (emacs-startup . global-jinx-mode)
  :bind (([remap ispell-word] . jinx-correct)
		 ("M-i" . jinx-correct)
		 ("M-o" . jinx-previous)
		 ("M-p" . jinx-next))
  :config
  (setq jinx-languages "en")
  ;; The quick broown fox jumps uver the lazy dog.
  (set-face-attribute 'jinx-misspelled nil :underline '(:color "#006800" :style wave)))

(use-package gcmh
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-low-cons-threshold minimal-emacs-gc-cons-threshold))
