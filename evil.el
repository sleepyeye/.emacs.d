(use-package evil
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-fine-undo t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  ;; copy current pos to EOL instead copy the whole line.
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-respect-visual-line-mode t)
  :config
  ;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;; (global-set-key (kbd "C-g") 'keyboard-escape-quit)
  ;; (global-set-key (kbd "<escape>") 'keyboard-quit)
  (evil-set-command-property 'xref-find-definitions :jump t)
  (evil-set-command-property 'xref-find-references :jump t)

  ;; Not a long-term VI user, so let's Emacsify some other keybindings:
  (define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-f") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-motion-state-map "_" 'evil-end-of-line)
  (define-key evil-motion-state-map "0" 'evil-beginning-of-line)


  ;; Even with the `evil-collections' (see below), some modes should be Emacs:
  (dolist (mode '(custom-mode
				  eshell-mode
				  git-rebase-mode
				  vterm-mode
				  elpaca-ui-mode
				  term-mode
				  calc-mode
				  inferior-python-mode))
	(add-to-list 'evil-emacs-state-modes mode))
  (evil-set-initial-state 'debugger-mode 'motion)
  (evil-set-initial-state 'pdf-view-mode 'motion)
  (evil-set-initial-state 'git-commit-mode 'insert)

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :demand t
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-surround
  :demand t
  :config
  (global-evil-surround-mode 1)
  :general
  (:states 'visual
           "s" 'evil-surround-region
           "S" 'evil-Surround-region)
  (:states 'operator
            "s" 'evil-surround-edit))

(use-package evil-commentary
  :demand t
  :config
  (evil-commentary-mode))

(use-package evil-lion
  :demand t
  :config
  (evil-lion-mode))

(use-package evil-textobj-line
  :demand t)

(use-package evil-args
  :demand t
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)

  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(use-package evil-exchange
  :init
  (setq evil-exchange-key (kbd "gx")
		evil-exchange-cancel-key (kbd "gX"))
  :config
  (evil-exchange-install))


(use-package undo-fu)

(use-package evil-mc
  :after evil
  :general
  (:states 'visual
		   "A" #'evil-mc-make-cursor-in-visual-selection-end
		   "I" #'evil-mc-make-cursor-in-visual-selection-beg)
  (:states 'normal
		   "M-n" #'evil-mc-make-and-goto-next-match
		   "Q"   #'evil-mc-undo-all-cursors)
  :config
  (global-evil-mc-mode 1))
