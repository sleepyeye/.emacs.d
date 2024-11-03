(use-package transient
  :defer t
  :config
  (transient-bind-q-to-quit))

(use-package magit
  :defer t
  :commands magit-status
  :general
  (sleepy/leader-def "gg" 'magit-status)
  :config
  ;; See https://github.com/magit/magit/issues/2124
  (defun mu-magit-kill-buffers (param)
	"Restore window configuration and kill all Magit buffers."
	(let ((buffers (magit-mode-get-buffers)))
	  (magit-restore-window-configuration)
	  (mapc #'kill-buffer buffers)))

  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
		magit-diff-hide-trailing-cr-characters t)
  (magit-auto-revert-mode t)
  (setq-default magit-bury-buffer-function #'mu-magit-kill-buffers)

  ;; Disable Emacs' built-in VC package for git repositories. This prevents it from doing unnecessary work when
  ;; Magit is performing git operations. This was recommended by the Magit manual. Empirically, I've noticed
  ;; this greatly speeds up git rebasing with Magit.
  (setq vc-handled-backends (delq 'Git vc-handled-backends))

  ;; Don't refresh the status buffer unless it's currently focused. This should improve performance.
  (setq magit-refresh-status-buffer nil))

(use-package diff-hl
  :demand t
  :init
  ;; Better looking colours for diff indicators /w spacemacs-light theme
  (custom-set-faces
  '(diff-hl-change ((t (:foreground "#553d00"))))
  '(diff-hl-insert ((t (:foreground "#005000"))))
  '(diff-hl-delete ((t (:foreground "#8f1313")))))
  :config
  (global-diff-hl-mode)
  (setq diff-hl-draw-borders nil)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(use-package git-timemachine
  :defer t
  :commands git-timemachine
  :config (sleepy/leader-def "gt" 'git-timemachine))
