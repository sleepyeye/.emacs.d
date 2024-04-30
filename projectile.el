(use-package projectile
  :demand t
  :after (general)
  :config
  ;;; override the keymap
  (sleepy/leader-def
	"SPC" 'projectile-find-file)

  (sleepy/leader-def
	"p-" 'projectile-dired
	"p_" 'projectile-dired-other-window
	"p!" 'projectile-run-shell-command-in-root
	"p%" 'projectile-replace-regexp
	"p&" 'projectile-run-async-shell-command-in-root
	"p-" 'projectile-dired
	"pf" 'projectile-find-file
	"pc" 'projectile-compile-project
	"pC" 'projectile-configure-project
	"pe" 'projectile-edit-dir-locals
	"pI" 'projectile-invalidate-cache
	"pk" 'projectile-kill-buffers
	"pR" 'projectile-replace
	"pS" 'projectile-save-project-buffers
	"pv" 'projectile-vc)
  :config
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "^\\build$")
  (add-to-list 'projectile-globally-ignored-directories "^\\elpa$")
  (add-to-list 'projectile-globally-ignored-directories "^\\url$")
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root t))


(use-package consult-projectile)
