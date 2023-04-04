(use-package magit
  :demand t
  :config
  (sleepy/leader-def
	"gg" 'magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
		magit-diff-hide-trailing-cr-characters t
		magit-save-window-configuration 'dontask)
  )





;; I combine it with this to get a zen-like full window Git status after switching projects:

;; (setq projectile-switch-project-action 'magit-status)
(use-package git-gutter
  :demand t
  :hook (prog-mode . git-gutter-mode)
  :init
  (setq git-gutter:disabled-modes '(org-mode asm-mode image-mode)
		git-gutter:window-width 1
		git-gutter:ask-p nil))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :after git-gutter
  :demand fringe-helper
  :config
  ;; subtle diff indicators in the fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  )


(use-package git-timemachine
  :commands git-timemachine
  :config
  (sleepy/leader-def
	"gt" 'git-timemachine))

