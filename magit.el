;;; magit.el --- Git interface configuration -*- lexical-binding: t; -*-

;; transient
(use-package transient
  :config
  (transient-bind-q-to-quit))

;; magit
(use-package magit
  :commands (magit-status magit-log-all)
  :init
  ;; Open status buffer in fullscreen
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-diff-hide-trailing-cr-characters t
        ;; Auto-save unsaved buffers without asking (faster for rebase, etc.)
        magit-save-repository-buffers 'dontask)
  :config
  ;; Restore window layout but keep buffer (recommended default behavior)
  (setq-default magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; Auto-reflect file changes (default, but explicitly stated)
  (magit-auto-revert-mode 1))

;; git-gutter (VC-independent, lightweight)
(use-package git-gutter
  :hook (emacs-startup . global-git-gutter-mode)
  :init
  ;; Minimize symbols to avoid visual clutter
  (setq git-gutter:modified-sign " "
        git-gutter:added-sign    " "
        git-gutter:deleted-sign  " ")
  :config
  ;; Too frequent updates increase CPU usage → 0.2-0.3s recommended
  (setq git-gutter:update-interval 0.2
        git-gutter:hide-gutter nil)
  ;; Manage colors with set-face-attribute (avoid custom-*)
  (set-face-attribute 'git-gutter:modified nil :foreground "#553d00" :weight 'bold)
  (set-face-attribute 'git-gutter:added    nil :foreground "#005000" :weight 'bold)
  (set-face-attribute 'git-gutter:deleted  nil :foreground "#8f1313" :weight 'bold))

;; timemachine
(use-package git-timemachine
  :defer t
  :commands git-timemachine)

(with-eval-after-load 'general
  (sleepy/leader-def
    "gd" 'git-timemachine-toggle      ;; View current file's history
    "gD" 'magit-diff-buffer-file      ;; Diff current file in Magit UI
    "gE" 'ediff-buffers))             ;; Manually diff current buffer vs timemachine buffer

;;; magit.el ends here
