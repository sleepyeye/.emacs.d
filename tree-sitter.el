;;; Emacs built-in treesitter package
(use-feature treesit
  :demand t
  :config
  (setq treesit-font-lock-level 4)
  ;; this package is not in MELPA
  ;; credit @renzmann
  (load "~/.emacs.d/treesit-auto.el"))


(elpaca-use-package evil-textobj-tree-sitter
  :after (treest evil)
  :demand t
  :config

  ;; Goto start of next function
  (define-key evil-normal-state-map (kbd "]f") (lambda ()
						 (interactive)
						 (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  ;; Goto start of previous function
  (define-key evil-normal-state-map (kbd "[f") (lambda ()
						 (interactive)
						 (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  ;; Goto end of next function
  (define-key evil-normal-state-map (kbd "]F") (lambda ()
						 (interactive)
						 (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  ;; Goto end of previous function
  (define-key evil-normal-state-map (kbd "[F") (lambda ()
						 (interactive)
						 (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

