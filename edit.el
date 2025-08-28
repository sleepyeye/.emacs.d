;;; edit.el --- editing utilities (lean & fast) -*- lexical-binding: t; -*-

;; iedit: 다중 동일 영역 편집 (필요할 때만 로드)
(use-package iedit
  :ensure t
  :commands (iedit-mode iedit-mode-toggle-on-function))

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
