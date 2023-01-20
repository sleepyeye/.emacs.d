(elpaca-use-package magit
  :demand t
  :general
  (sleepy/util-keys
   "g" 'magit-status)
  :config
  (setq magit-display-buffer-function
	#'magit-display-buffer-fullframe-status-v1)
  )




;; I combine it with this to get a zen-like full window Git status after switching projects:

;; (setq projectile-switch-project-action 'magit-status)

(elpaca-use-package git-gutter
  :demand t
  :config
  (global-git-gutter-mode +1))


(elpaca-use-package git-timemachine
  :commands git-timemachine
  :general
  (sleepy/util-keys
   "t" 'git-timemachine))

