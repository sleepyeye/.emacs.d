(defun sleepy/python-capf ()
  (setq-local completion-at-point-functions
			  (list (cape-super-capf
					 #'tempel-expand
					 #'eglot-completion-at-point
					 #'cape-keyword
					 #'cape-dabbrev
					 ))))


(add-hook 'python-mode #'sleepy/python-capf)
(add-hook 'python-ts-mode #'sleepy/python-capf)

(use-package python
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  :config
  (when (and (executable-find "python3")
			 (string= python-shell-interpreter "python"))
	(setq python-shell-interpreter "python3"))
  )

(use-package python-black
  :after python
  :hook ((python-mode . python-black-on-save-mode-enable-dwim)
		 (python-ts-mode . python-black-on-save-mode-enable-dwim))
  :config
  (general-evil-define-key '(normal) python-ts-mode-map
	"=" #'python-black-region)

  (general-evil-define-key '(normal) python-mode-map
	"=" #'python-black-region))


(use-package conda
  :defer t
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)

  :init
  (setq conda-anaconda-home (expand-file-name "~/miniforge3"))
  (setq conda-env-home-directory (expand-file-name "~/miniforge3")))
