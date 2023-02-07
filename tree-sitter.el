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
  (setq treesit-font-lock-level 4))

(elpaca-use-package (treesit-auto :host github :repo "sleepyeye/treesit-auto" :protocol https)
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(elpaca-use-package (combobulate :host github :repo "mickeynp/combobulate" :protocol https)
	:after treesit
	:hook
	((python-ts-mode . combobulate-mode)
	 (js-ts-mode . combobulate-mode)
	 (css-ts-mode . combobulate-mode)
	 (yaml-ts-mode . combobulate-mode)
	 (typescript-ts-mode . combobulate-mode)
	 (tsx-ts-mode . combobulate-mode))
	:general
	(general-define-key
	 :keymaps 'combobulate-key-map
	 "M-a" #'combobulate-navigate-beginning-of-defun
	 "M-e" #'combobulate-navigate-end-of-defun
	 "M-h" #'combobulate-navigate-up-list-maybe
	 "M-j" #'combobulate-navigate-next
	 "M-k" #'combobulate-navigate-previous
	 "M-l" #'combobulate-navigate-down-list-maybe

	 "M-J" #'combobulate-drag-down
	 "M-K" #'combobulate-drag-up

	 "M-m" #'combobulate-edit-cluster-dwim
	 ))


(elpaca-use-package evil-textobj-tree-sitter
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

