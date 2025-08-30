;; transient
(use-package transient
  :config
  (transient-bind-q-to-quit))

;; magit
(use-package magit
  :commands (magit-status magit-log-all)
  :init
  ;; 상태 버퍼는 전체화면으로 열기
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-diff-hide-trailing-cr-characters t
        ;; 저장 안 된 버퍼 자동 저장 묻지 않고 진행 (rebase 등 빠르게)
        magit-save-repository-buffers 'dontask)
  :config
  ;; 창 배치는 복원하되, 버퍼는 남겨두는 기본 동작 권장
  (setq-default magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; 파일 변경 자동 반영(기본값이지만 명시해 둠)
  (magit-auto-revert-mode 1))

;; git-gutter (VC 비의존, 가벼움)
(use-package git-gutter
  :hook (emacs-startup . global-git-gutter-mode)
  :init
  ;; 기호는 눈에 거슬리지 않게 최소화
  (setq git-gutter:modified-sign " "
        git-gutter:added-sign    " "
        git-gutter:deleted-sign  " ")
  :config
  ;; 너무 촘촘하면 CPU 사용량↑ → 0.3s 정도 권장
  (setq git-gutter:update-interval 0.2
        git-gutter:hide-gutter nil)
  ;; 색상은 set-face-attribute로 관리 (custom-* 지양)
  (set-face-attribute 'git-gutter:modified nil :foreground "#553d00" :weight 'bold)
  (set-face-attribute 'git-gutter:added    nil :foreground "#005000" :weight 'bold)
  (set-face-attribute 'git-gutter:deleted  nil :foreground "#8f1313" :weight 'bold))

;; timemachine
(use-package git-timemachine
  :defer t
  :commands git-timemachine)

(with-eval-after-load 'general
  (sleepy/leader-def
    "gd" 'git-timemachine-toggle      ;; 현재 파일의 과거 기록 보기
    "gD" 'magit-diff-buffer-file      ;; 현재 파일 Magit UI에서 diff
    "gE" 'ediff-buffers))             ;; 수동으로 현재 버퍼 vs timemachine 버퍼 diff
