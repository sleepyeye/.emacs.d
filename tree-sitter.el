;;; Emacs built-in treesitter package
(use-package treesit
  :elpaca nil
  :demand t
  :config
  ;; allocate 512MB for treesit buffer
  (setq treesit-max-buffer-size (* 512 1024 1024))
  (setq treesit-font-lock-level 3))

;;; Grammer instllation helper
(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))
