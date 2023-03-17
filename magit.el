(use-package magit
  :demand t
  :config
  (sleepy/leader-def
   "gg" 'magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
				magit-diff-hide-trailing-cr-characters t)
  )




;; I combine it with this to get a zen-like full window Git status after switching projects:

;; (setq projectile-switch-project-action 'magit-status)

;; (use-package git-gutter
;;   :demand t
;;   :config
;;  (setq git-gutter:disabled-modes '(org-mode asm-mode image-mode)
;;         git-gutter:update-interval 0.1
;;         git-gutter:window-width 1
;;         git-gutter:ask-p nil)
;;   (global-git-gutter-mode +1))

;; (use-package git-gutter-fringe
;;   :diminish git-gutter-mode
;;   :after git-gutter
;;   :demand fringe-helper
;; 	:config
;;   ;; subtle diff indicators in the fringe
;;   ;; places the git gutter outside the margins.
;;   (setq-default fringes-outside-margins t)
;; 	)


(use-package git-timemachine
  :commands git-timemachine
  :config
  (sleepy/leader-def
   "gt" 'git-timemachine))

