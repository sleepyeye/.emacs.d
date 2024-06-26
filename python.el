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
  (when (and (executable-find "python3")
			 (string= python-shell-interpreter "python"))
	(setq python-shell-interpreter "python3")))

(use-package python-black
  :after python
  :general
  (:keymaps 'python-mode-map :states 'normal "=" #'python-black-region)
  (:keymaps 'python-ts-mode-map :states 'normal "=" #'python-black-region))
