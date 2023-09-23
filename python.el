(defun sleepy/python-capf ()
  (setq-local completion-at-point-functions
			  (list (cape-super-capf
					 #'tempel-expand
					 #'eglot-completion-at-point
					 #'cape-keyword
					 #'cape-dabbrev
					 ))))


;; Open python files in tree-sitter mode.
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(add-hook 'python-mode #'sleepy/python-capf)
(add-hook 'python-ts-mode #'sleepy/python-capf)

(use-package python
  :elpaca nil
  :init
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


(use-package conda
  :defer t
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))
