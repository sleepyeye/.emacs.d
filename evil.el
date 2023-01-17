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
