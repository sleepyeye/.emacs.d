;;; Emacs built-in treesitter package
(use-feature treesit
  :demand t
  :config
  (defun treesit-mark-bigger-node ()
    (let* ((root (treesit-buffer-root-node))
	   (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
	   (node-start (treesit-node-start node))
	   (node-end (treesit-node-end node)))
      ;; Node fits the region exactly. Try its parent node instead.
      (when (and (= (region-beginning) node-start) (= (region-end) node-end))
	(when-let ((node (treesit-node-parent node)))
	  (setq node-start (treesit-node-start node)
		node-end (treesit-node-end node))))
      (set-mark node-end)
      (goto-char node-start)))
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

