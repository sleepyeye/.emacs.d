;;; projectile.el --- lean, git-root only, standard keybinds -*- lexical-binding: t; -*-

(use-package projectile
  :ensure t
  :init
  ;; ✅ Git 루트만 프로젝트로 인정 (원치 않는 상위 폴더 자동 등록 방지)
  (setq projectile-project-root-files-bottom-up '(".git")
        projectile-project-root-files-top-down nil
        projectile-project-root-files-top-down-recurring nil)

  ;; ✅ 파일 열었다고 자동으로 known-projects에 추가하지 않음
  (setq projectile-track-known-projects-automatically nil)

  ;; ✅ 프로젝트 루트가 아닐 땐 Projectile 명령 자체가 동작하지 않게
  (setq projectile-require-project-root t)

  ;; ✅ 검색 경로/깊이: ~/workspace 바로 아래 2단계까지만
  (setq projectile-project-search-path '(("~/workspace" . 2)))

  ;; ✅ 캐시 디렉토리 명시적 설정 (홈 폴더 대신 ~/.emacs.d/projectile-cache)
  (setq projectile-cache-file
        (expand-file-name "projectile.cache" user-emacs-directory)
        projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

  ;; ✅ ~/workspace 외부 디렉토리는 프로젝트로 인식하지 않음
  (setq projectile-ignored-projects
        (list "~/" "/tmp/" "/usr/" "/opt/" "/etc/"
              (expand-file-name "~/.emacs.d/")
              (expand-file-name "~/.config/")))

  ;; ✅ ~/workspace 외부의 모든 디렉토리를 프로젝트에서 제외
  (setq projectile-ignored-project-function
        (lambda (project-root)
          (let ((workspace-path (expand-file-name "~/workspace/")))
            ;; ~/workspace/ 아래가 아니면 무시
            (not (string-prefix-p workspace-path project-root)))))

  ;; ✅ 인덱싱/캐시: 대체로 가장 빠른 조합
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t)

  ;; ✅ 완료 시스템은 Emacs 기본(vertico/orderless와 충돌 없음)
  (setq projectile-completion-system 'default)

  ;; ripgrep 쓰면 파일 목록 명령도 rg로
  (when (executable-find "rg")
    (setq projectile-generic-command "rg --files --hidden --follow --color=never -0"
          projectile-git-command     "rg --files --hidden --follow --color=never -z"))

  :config
  (projectile-mode 1)

  ;; 전역 무시 디렉터리(이름 기준) — 정규식보다 안전
  (dolist (name '("build" "dist" "target" "node_modules" "__pycache__"
                  ".venv" ".direnv" "elpa" "url"))
    (add-to-list 'projectile-globally-ignored-directories name))

  ;; 프로젝트 전환 시 동작(기본은 dired). 선호하면 find-file로 바꾸기:
  ;; (setq projectile-switch-project-action #'projectile-find-file)

  ;; ── 편의 함수: 루트 todo.org 열기/생성 ─────────────────────────────
  (defun open-project-todo ()
    "Open or create `todo.org` at current Projectile project root."
    (interactive)
    (let ((root (projectile-project-root)))
      (unless root (user-error "Not in a Projectile project"))
      (let ((path (expand-file-name "todo.org" root)))
        (unless (file-exists-p path)
          (with-temp-buffer
            (insert "#+TITLE: TODOs for "
                    (file-name-nondirectory (directory-file-name root)) "\n\n")
            (write-file path)))
        (find-file path))))

  ;; Commander에 't' 추가: M-x projectile-commander → t
  (add-to-list 'projectile-commander-methods
               '(?t "Open or create todo.org at project root" open-project-todo))

  ;; ── 표준 키바인딩(C-c p …) 중 자주 쓰는 것만 명시 ─────────────────
  (let ((map projectile-command-map))
    (define-key map (kbd "p") #'projectile-switch-project)
    (define-key map (kbd "f") #'projectile-find-file)
    (define-key map (kbd "d") #'projectile-find-dir)
    (define-key map (kbd "b") #'projectile-switch-to-buffer)
    (define-key map (kbd "k") #'projectile-kill-buffers)
    (define-key map (kbd "r") #'projectile-recentf)
    (define-key map (kbd "s r") #'projectile-ripgrep)
    (define-key map (kbd ":") #'projectile-commander)
    (define-key map (kbd "'") #'open-project-todo))

  ;; 리더키 연동(있을 때만): SPC p → projectile-command-map
  (when (fboundp 'sleepy/leader-def)
    (sleepy/leader-def "p" '(projectile-command-map :which-key "Project"))))

(use-package consult-projectile
  :after (projectile consult)
  :ensure t)

;;; projectile.el ends here
