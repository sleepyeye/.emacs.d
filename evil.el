;;; evil.el --- lean evil setup using package defaults (no :general) -*- lexical-binding: t; -*-

;; ---- Undo stack -------------------------------------------------------------
(use-package undo-fu :ensure t)
(use-package undo-fu-session
  :ensure t
  :init (undo-fu-session-global-mode 1))

;; ---- Evil core --------------------------------------------------------------
(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-fu
        evil-want-Y-yank-to-eol t
        evil-respect-visual-line-mode t
        evil-want-fine-undo t)
  :config
  (evil-mode 1)

  ;; 검색 모듈
  (evil-select-search-module 'evil-search-module 'isearch)

  ;; 점프 커맨드로 표시
  (evil-set-command-property 'xref-find-definitions :jump t)
  (evil-set-command-property 'xref-find-references :jump t)

  ;; Emacs화 키 몇 가지 (원래 설정 유지)
  (define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-f") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-line)
  (define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "C-A") 'evil-beginning-of-visual-line)
  (define-key evil-normal-state-map (kbd "C-E") 'evil-end-of-visual-line)
  (define-key evil-motion-state-map "_" 'evil-end-of-line)
  (define-key evil-motion-state-map "0" 'evil-beginning-of-line)

  ;; Evil 상태 예외
  (dolist (mode '(custom-mode eshell-mode shell-mode term-mode vterm-mode
                  elpaca-ui-mode calc-mode inferior-python-mode wdired-mode
                  log-edit-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  (evil-set-initial-state 'debugger-mode 'motion)
  (evil-set-initial-state 'pdf-view-mode 'motion)
  (evil-set-initial-state 'git-commit-mode 'insert)

  ;; 댓글 토글 연산자: gc
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for region."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

;; ---- Evil collection (패키지 기본 evil 바인딩 사용) ------------------------
(use-package evil-collection
  :after evil
  :ensure t
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

;; ---- Surround ---------------------------------------------------------------
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1)
  ;; (use-package의 :general 없이 general-define-key 사용)
  (when (featurep 'general)
    (general-define-key
     :states 'visual
     "s" 'evil-surround-region
     "S" 'evil-Surround-region)
    (general-define-key
     :states 'operator
     "s" 'evil-surround-edit)))

;; ---- 기타 텍스트 오브젝트/도구 ---------------------------------------------
(use-package evil-textobj-line :ensure t :after evil)

(use-package evil-args
  :ensure t
  :after evil
  :config
  ;; text object
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  ;; 이동/점프
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

(use-package evil-exchange
  :ensure t
  :init
  (setq evil-exchange-key (kbd "gx")
        evil-exchange-cancel-key (kbd "gX"))
  :config
  (evil-exchange-install))

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-visualstar
  :ensure t
  :after evil
  :commands global-evil-visualstar-mode
  :hook (after-init . global-evil-visualstar-mode))

;; ---- vdiff (옵션) ----------------------------------------------------------
(use-package vdiff
  :ensure t
  :commands (vdiff-buffers vdiff-buffers3 vdiff-quit vdiff-files vdiff-files3)
  :custom
  (vdiff-auto-refine t)
  (vdiff-only-highlight-refinements t))

;; ---- Motion 맵 정리 --------------------------------------------------------
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

;; ---- Better-jumper (C-i 충돌 방지) -----------------------------------------
(use-package better-jumper
  :ensure t
  :init (better-jumper-mode 1)
  :config
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
    ;; <C-i>는 TAB과 충돌 가능 → 대체 키
    (define-key evil-motion-state-map (kbd "M-]") 'better-jumper-jump-forward)))

;; ---- Tree-sitter text objects (선택) ----------------------------------------
(use-package evil-textobj-tree-sitter
  :ensure t
  :after evil
  :config

  ;; function
  (define-key evil-outer-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-normal-state-map (kbd "]f")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  (define-key evil-normal-state-map (kbd "[f")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  (define-key evil-normal-state-map (kbd "]F")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[F")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))

  ;; class
  (define-key evil-outer-text-objects-map "g"
    (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "g"
    (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-normal-state-map (kbd "]g")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer")))
  (define-key evil-normal-state-map (kbd "[g")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t)))
  (define-key evil-normal-state-map (kbd "]G")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[G")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t t))))
