(use-package magit
  :demand t
  :config
  (sleepy/leader-def
	"gg" 'magit-status)
  :config

  ;; See https://github.com/magit/magit/issues/2124
  (defun mu-magit-kill-buffers (param)
	"Restore window configuration and kill all Magit buffers."
	(let ((buffers (magit-mode-get-buffers)))
	  (magit-restore-window-configuration)
	  (mapc #'kill-buffer buffers)))

  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
		magit-diff-hide-trailing-cr-characters t)
  (setq-default magit-bury-buffer-function #'mu-magit-kill-buffers)
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

  ;; Make fringe more modern
  (setq git-gutter-fr:side 'left-fringe)
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated))
  )


(use-package git-timemachine
  :commands git-timemachine
  :config
  (sleepy/leader-def
	"gt" 'git-timemachine))
