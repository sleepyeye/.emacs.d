(use-package magit
  :demand t
  :config
  (sleepy/util-keys
   "g" 'magit-status)
  :config
  (setq magit-display-buffer-function
	#'magit-display-buffer-fullframe-status-v1)
  )




;; I combine it with this to get a zen-like full window Git status after switching projects:

;; (setq projectile-switch-project-action 'magit-status)

(use-package git-gutter
  :demand t
  :config
	(setq git-gutter:update-interval 0.02)
  (global-git-gutter-mode +1))


(use-package git-timemachine
  :commands git-timemachine
  :config
  (sleepy/util-keys
   "t" 'git-timemachine))

