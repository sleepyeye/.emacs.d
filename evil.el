(elpaca-use-package evil
  :demand t
  :init
  (setq evil-want-integration t) 
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(elpaca-use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))
