(elpaca-use-package evil
  :demand t
  :init
  (setq evil-want-integration t) 
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-set-key (kbd "C-g") 'keyboard-escape-quit)
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

  ;; ;; Even with the `evil-collections' (see below), some modes should be Emacs:
  ;; (dolist (mode '(custom-mode
  ;;                 eshell-mode
  ;;                 git-rebase-mode
  ;;                 vterm-mode))
  ;;   (add-to-list 'evil-emacs-state-modes mode))

  (evil-mode 1))

(elpaca-use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

(elpaca-use-package evil-surround
  :demand t
  :config
  (global-evil-surround-mode 1))


(elpaca-use-package evil-commentary
  :demand t
  :config
  (evil-commentary-mode))

(elpaca-use-package evil-lion
  :demand t
  :config
  (evil-lion-mode))

(elpaca-use-package evil-textobj-line
  :demand t)

(elpaca-use-package evil-args
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

(elpaca-use-package evil-exchange
  :init
  (setq evil-exchange-key (kbd "gx")
        evil-exchange-cancel-key (kbd "gX"))
  :config
  (evil-exchange-install))


(elpaca-use-package undo-fu)
