(use-package transient
  :defer t
  :config
  (transient-bind-q-to-quit))

(use-package magit
  :defer t
  :commands magit-status
  :config
  ;; See https://github.com/magit/magit/issues/2124
  (defun mu-magit-kill-buffers (param)
	"Restore window configuration and kill all Magit buffers."
	(let ((buffers (magit-mode-get-buffers)))
	  (magit-restore-window-configuration)
	  (mapc #'kill-buffer buffers)))

  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
		magit-diff-hide-trailing-cr-characters t)
  (magit-auto-revert-mode 1)
  (setq-default magit-bury-buffer-function #'mu-magit-kill-buffers)

  ;;; I had to disable this since diff-hl relies on vc
  ;; ;; Disable Emacs' built-in VC package for git repositories. This prevents it from doing unnecessary work when
  ;; ;; Magit is performing git operations. This was recommended by the Magit manual. Empirically, I've noticed
  ;; ;; this greatly speeds up git rebasing with Magit.
  ;; (setq vc-handled-backends (delq 'Git vc-handled-backends))

  ;; Don't refresh the status buffer unless it's currently focused. This should improve performance.
  (setq magit-refresh-status-buffer nil))

(use-package git-gutter
  :demand t
  :hook (emacs-startup . global-git-gutter-mode)
  :init
  (custom-set-variables
   '(git-gutter:modified-sign " ")
   '(git-gutter:added-sign " ")
   '(git-gutter:deleted-sign " "))

  (custom-set-faces
   '(git-gutter:modified ((t (:foreground "#553d00" :weight bold))))
   '(git-gutter:added    ((t (:foreground "#005000" :weight bold))))
   '(git-gutter:deleted  ((t (:foreground "#8f1313" :weight bold)))))
  :config
  (setq git-gutter:update-interval 1)
  (setq git-gutter:hide-gutter nil))


(use-package git-timemachine
  :defer t
  :commands git-timemachine)
