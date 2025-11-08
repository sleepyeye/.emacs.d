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
        magit-save-repository-buffers 'dontask
        ;; Diff refinement - highlight word-level changes in all hunks
        magit-diff-refine-hunk 'all
        ;; Log display settings (show age, width, time format)
        magit-log-margin '(t age magit-log-margin-width t 18))
  :config
  ;; Restore window layout but keep buffer (recommended default behavior)
  (setq-default magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; Auto-reflect file changes (default, but explicitly stated)
  (magit-auto-revert-mode 1))

;; Git commit message best practices
(use-package git-commit
  :ensure nil
  :after magit
  :config
  ;; Enforce commit message conventions
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 72
        git-commit-style-convention-checks '(non-empty-second-line
                                              overlong-summary-line)))

;; Magit-todos: Show TODO/FIXME in magit status
(use-package magit-todos
  :ensure t
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :config
  ;; Customize appearance
  (setq magit-todos-keywords-list '("TODO" "FIXME" "HACK" "XXX" "NOTE")
        magit-todos-exclude-globs '("*.map" "*.min.js" "*.lock")
        magit-todos-branch-list nil  ; Only scan current branch
        magit-todos-recursive t
        magit-todos-depth 100)
  ;; Use ripgrep if available for better performance
  (when (executable-find "rg")
    (setq magit-todos-scanner 'magit-todos--scan-with-ripgrep)))

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
