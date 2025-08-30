;;;; workspace.el --- perspective + consult integration -*- lexical-binding: t; -*-

(use-package perspective
  :demand t
  :init
  (setq persp-state-default-file (expand-file-name ".persp" user-emacs-directory)
        persp-suppress-no-prefix-key-warning t)
  :config
  (persp-mode 1)

  ;; 세션 저장/복원
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (when (file-exists-p persp-state-default-file)
    (ignore-errors (persp-state-load persp-state-default-file)))

  ;; ── 숫자 워크스페이스 전환 ──
  (defun sleepy/persp--switch (name)
    "Switch to perspective NAME (create if missing)."
    (interactive) (persp-switch name))

  (defun sleepy/persp-0 () (interactive) (sleepy/persp--switch "0"))
  (defun sleepy/persp-1 () (interactive) (sleepy/persp--switch "1"))
  (defun sleepy/persp-2 () (interactive) (sleepy/persp--switch "2"))
  (defun sleepy/persp-3 () (interactive) (sleepy/persp--switch "3"))
  (defun sleepy/persp-4 () (interactive) (sleepy/persp--switch "4"))
  (defun sleepy/persp-5 () (interactive) (sleepy/persp--switch "5"))
  (defun sleepy/persp-6 () (interactive) (sleepy/persp--switch "6"))
  (defun sleepy/persp-7 () (interactive) (sleepy/persp--switch "7"))
  (defun sleepy/persp-8 () (interactive) (sleepy/persp--switch "8"))
  (defun sleepy/persp-9 () (interactive) (sleepy/persp--switch "9"))

  ;; ── 프로젝트 진입 시 자동 워크스페이스 ──
  (defun sleepy/persp-for-project ()
    "Switch to a workspace named after the current project."
    (when-let ((proj (project-current)))
      (let ((name (file-name-nondirectory
                   (directory-file-name (project-root proj)))))
        (persp-switch name))))
  (add-hook 'project-switch-project-hook #'sleepy/persp-for-project)

  ;; ── 이름으로 전환 ──
  (defun sleepy/persp-switch-completing ()
    "Switch to workspace chosen via minibuffer completion."
    (interactive)
    (persp-switch (completing-read "Switch to workspace: " (persp-names))))

  ;; ── consult: 현재 워크스페이스 버퍼만 노출 ──
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (defvar consult--source-perspective
      (list :name     "Workspace"
            :narrow   ?w
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            ;; 안전하게 람다로 현재 persp 버퍼 이름 반환
            :items    (lambda ()
                        (mapcar #'buffer-name
                                (ignore-errors
                                  (persp-buffers (persp-curr)))))))
    (add-to-list 'consult-buffer-sources 'consult--source-perspective t))


  ;; ── Workspace leader: SPC TAB … ──
  (with-eval-after-load 'general
	(sleepy/leader-def
      ;; ── 숫자 워크스페이스 전환 ──
      "TAB 1" #'sleepy/persp-1
      "TAB 2" #'sleepy/persp-2
      "TAB 3" #'sleepy/persp-3
      "TAB 4" #'sleepy/persp-4
      "TAB 5" #'sleepy/persp-5
      "TAB 6" #'sleepy/persp-6
      "TAB 7" #'sleepy/persp-7
      "TAB 8" #'sleepy/persp-8
      "TAB 9" #'sleepy/persp-9
      "TAB 0" #'sleepy/persp-0

      ;; ── 워크스페이스 유틸 ──
      "TAB TAB" #'sleepy/persp-switch-completing))
  )
