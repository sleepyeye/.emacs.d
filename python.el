(defun sleepy/python-capf ()
  (setq-local completion-at-point-functions
			  (list (cape-capf-super
					 #'tempel-complete
					 #'eglot-completion-at-point
					 #'cape-keyword))))


;; Open python files in tree-sitter mode.
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-hook 'python-mode-hook (lambda () (add-hook 'eglot-managed-mode-hook #'sleepy/python-capf)))
(add-hook 'python-ts-mode-hook (lambda () (add-hook 'eglot-managed-mode-hook #'sleepy/python-capf)))

(use-package python
  :ensure nil
  :init
  (setq python-indent-offset standard-indent)
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  :config
  (when (and (executable-find (expand-file-name"~/miniconda3/bin/python3"))
			 (string= python-shell-interpreter "python"))
	(setq python-shell-interpreter "python3")))

(use-package conda
  :init
  (setq conda-env-home-directory (expand-file-name "~/miniconda3/")
		conda-env-subdirectory "envs")
  (setq conda-env-autoactivate-mode t)
  (add-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
										 (conda-env-activate-for-buffer)))))

;; (use-package python-black
;;   :after python
;;   :general
;;   (:keymaps 'python-mode-map :states 'normal "=" #'python-black-region)
;;   (:keymaps 'python-ts-mode-map :states 'normal "=" #'python-black-region))
