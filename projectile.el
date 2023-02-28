(use-package projectile
  :demand t
  :after (general)
	:config

  ;;; override the keymap
  (sleepy/leader-keys
    "SPC" 'projectile-find-file)

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
  (projectile-mode)
	(add-to-list 'projectile-globally-ignored-directories "^\\build$")
	(add-to-list 'projectile-globally-ignored-directories "^\\elpa$")
	(add-to-list 'projectile-globally-ignored-directories "^\\url$")
	(setq projectile-enable-caching t)
	(setq projectile-require-project-root t)


	)


(use-package consult-projectile)
