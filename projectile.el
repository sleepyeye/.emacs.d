(elpaca-use-package projectile
  :demand t
  :after (general)
  :general
  (sleepy/project-keys
    "!" 'projectile-run-shell-command-in-root
    "%" 'projectile-replace-regexp
    "&" 'projectile-run-async-shell-command-in-root
    "-" 'projectile-dired
    "f" 'projectile-find-file
    "s" 'projectile-rg
    "c" 'projectile-compile-project
    "C" 'projectile-configure-project
    "e" 'projectile-edit-dir-locals
    "I" 'projectile-invalidate-cache
    "k" 'projectile-kill-buffers
    "R" 'projectile-replace
    "S" 'projectile-save-project-buffers
    "v" 'projectile-vc)
  :config
  (projectile-mode))


;; (elpaca-use-package consult-projectile)
