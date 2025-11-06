;;; edit.el --- editing utilities (lean & fast) -*- lexical-binding: t; -*-

;; iedit: 다중 동일 영역 편집 (필요할 때만 로드)
(use-package iedit
  :ensure t
  :commands (iedit-mode iedit-mode-toggle-on-function))

;; evil-mc: multiple cursors for Evil (수동 커서 배치)
(use-package evil-mc
  :ensure t
  :after evil
  :config
  (global-evil-mc-mode 1)

  ;; evil-mc 키바인딩 (Evil과 충돌 방지)
  (define-key evil-normal-state-map (kbd "C-M-j") 'evil-mc-make-cursor-move-next-line)
  (define-key evil-normal-state-map (kbd "C-M-k") 'evil-mc-make-cursor-move-prev-line)
  (define-key evil-visual-state-map (kbd "C-M-j") 'evil-mc-make-cursor-move-next-line)
  (define-key evil-visual-state-map (kbd "C-M-k") 'evil-mc-make-cursor-move-prev-line)

  ;; grm: 모든 매치에 커서 생성
  (define-key evil-normal-state-map (kbd "grm") 'evil-mc-make-all-cursors)
  ;; gru: 커서 모두 제거
  (define-key evil-normal-state-map (kbd "gru") 'evil-mc-undo-all-cursors)
  ;; grn: 다음 매치에 커서 추가
  (define-key evil-normal-state-map (kbd "grn") 'evil-mc-make-and-goto-next-match)
  ;; grp: 이전 매치에 커서 추가
  (define-key evil-normal-state-map (kbd "grp") 'evil-mc-make-and-goto-prev-match)
  ;; grN: 다음 매치 스킵
  (define-key evil-normal-state-map (kbd "grN") 'evil-mc-skip-and-goto-next-match)
  ;; grP: 이전 매치 스킵
  (define-key evil-normal-state-map (kbd "grP") 'evil-mc-skip-and-goto-prev-match)

  ;; 일시 정지/재개
  (define-key evil-normal-state-map (kbd "grq") 'evil-mc-pause-cursors)
  (define-key evil-normal-state-map (kbd "grr") 'evil-mc-resume-cursors))

(use-package ialign
  :ensure t)

(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode wgrep-finish-edit)
  :config
  (setq wgrep-change-readonly-file t
        wgrep-auto-save-buffer t))

(use-package expand-region
  :ensure t
  :commands (er/expand-region er/contract-region)
  :bind (("M-=" . er/expand-region)
         ("M-+" . er/expand-region)
         ("M--" . er/contract-region)))

(use-package tempel
  :ensure t
  :commands (tempel-expand tempel-insert)
  :init
  (defun sleepy/tempel-capf-setup ()
    (add-hook 'completion-at-point-functions #'tempel-complete -10 t))
  :hook ((prog-mode . sleepy/tempel-capf-setup)
         (text-mode . sleepy/tempel-capf-setup))
  :custom
  (tempel-trigger-prefix "<")
  :bind
  (("M-+" . tempel-expand)
   :map tempel-map
   ("<tab>"    . tempel-next)
   ("<backtab>". tempel-previous)
   ("C-["      . tempel-next)
   ("C-]"      . tempel-previous)
   ("C-g"      . tempel-abort)
   ("C-k"      . tempel-kill)))

(use-package tempel-collection
  :ensure t
  :after tempel)

;;; edit.el ends here
