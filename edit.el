;;; edit.el --- editing utilities (lean & fast) -*- lexical-binding: t; -*-

;; iedit: 다중 동일 영역 편집 (필요할 때만 로드)
(use-package iedit
  :ensure t
  :commands (iedit-mode iedit-mode-toggle-on-function))

;; evil-mc: multiple cursors for Evil (수동 커서 배치)
(use-package evil-mc
  :ensure t
  :after (evil general)
  :config
  (global-evil-mc-mode 1)

  ;; Evil 스타일: gm 프리픽스 (multiple cursors - 다른 패키지들과 일관성 유지)
  (general-define-key
   :states 'normal
   "gmm" 'evil-mc-make-all-cursors
   "gmu" 'evil-mc-undo-all-cursors
   "gmn" 'evil-mc-make-and-goto-next-match
   "gmp" 'evil-mc-make-and-goto-prev-match
   "gmN" 'evil-mc-skip-and-goto-next-match
   "gmP" 'evil-mc-skip-and-goto-prev-match
   "gmq" 'evil-mc-pause-cursors
   "gmr" 'evil-mc-resume-cursors)

  ;; 줄 단위 커서 추가 (normal & visual)
  (general-define-key
   :states '(normal visual)
   "C-M-j" 'evil-mc-make-cursor-move-next-line
   "C-M-k" 'evil-mc-make-cursor-move-prev-line)

  ;; Emacs 스타일: C-c m 프리픽스 (전역)
  (general-define-key
   "C-c m j" 'evil-mc-make-cursor-move-next-line
   "C-c m k" 'evil-mc-make-cursor-move-prev-line
   "C-c m n" 'evil-mc-make-and-goto-next-match
   "C-c m p" 'evil-mc-make-and-goto-prev-match
   "C-c m m" 'evil-mc-make-all-cursors
   "C-c m u" 'evil-mc-undo-all-cursors
   "C-c m q" 'evil-mc-pause-cursors
   "C-c m r" 'evil-mc-resume-cursors))

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
