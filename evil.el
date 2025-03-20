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
  (evil-select-search-module 'evil-search-module 'isearch)

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
  (define-key evil-normal-state-map (kbd "C-A") 'evil-beginning-of-visual-line)
  (define-key evil-normal-state-map (kbd "C-E") 'evil-end-of-visual-line)
  (define-key evil-motion-state-map "_" 'evil-end-of-line)
  (define-key evil-motion-state-map "0" 'evil-beginning-of-line)


  ;; Even with the `evil-collections' (see below), some modes should be Emacs:
  (dolist (mode '(custom-mode
				  eshell-mode
				  shell-mode
				  term-mode
				  vterm-mode
				  elpaca-ui-mode
				  term-mode
				  calc-mode
				  inferior-python-mode
				  wdired-mode
				  log-edit-mode))
	(add-to-list 'evil-emacs-state-modes mode))
  (evil-set-initial-state 'debugger-mode 'motion)
  (evil-set-initial-state 'pdf-view-mode 'motion)
  (evil-set-initial-state 'git-commit-mode 'insert)

  (evil-mode 1)


  ;;; replacement for evil-commentary
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
	"Toggle comment for the region between BEG and END."
	(interactive "<r>")
	(comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

(use-package undo-fu
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :custom
  ;; 3 times the default values
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode))

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


(use-package evil-multiedit
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-visualstar
  :after evil
  :ensure t
  :defer t
  :commands global-evil-visualstar-mode
  :hook (after-init . global-evil-visualstar-mode))

(use-package vdiff
  :ensure t
  :defer t
  :commands (vdiff-buffers
             vdiff-buffers3
             vdiff-quit
             vdiff-files
             vdiff-files3)
  :custom
  (vdiff-auto-refine t)
  (vdiff-only-highlight-refinements t))


(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))
