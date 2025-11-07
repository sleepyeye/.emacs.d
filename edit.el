;;; edit.el --- editing utilities (lean & fast) -*- lexical-binding: t; -*-

;; iedit: multiple identical region editing (loaded on demand)
(use-package iedit
  :ensure t
  :commands (iedit-mode iedit-mode-toggle-on-function))

;; evil-mc: multiple cursors for Evil (manual cursor placement)
(use-package evil-mc
  :ensure t
  :after (evil general)
  :config
  (global-evil-mc-mode 1)

  ;; Evil style: gm prefix (multiple cursors - consistent with other packages)
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

  ;; Line-wise cursor addition (normal & visual)
  (general-define-key
   :states '(normal visual)
   "C-M-j" 'evil-mc-make-cursor-move-next-line
   "C-M-k" 'evil-mc-make-cursor-move-prev-line)

  ;; Emacs style: C-c m prefix (global)
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

;;; edit.el ends here
