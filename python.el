(use-package python
	:after projectile 
	:init
	(setq python-indent-guess-indent-offset-verbose nil)
	:config
	(when (and (executable-find "python3")
						 (string= python-shell-interpreter "python"))
		(setq python-shell-interpreter "python3")))

(use-package python-black
	:after python
	:hook ((python-mode . python-black-on-save-mode-enable-dwim)
				 (python-ts-mode . python-black-on-save-mode-enable-dwim))
	:config
	(general-evil-define-key '(normal) python-ts-mode-map
		"=" #'python-black-region)

	(general-evil-define-key '(normal) python-mode-map
		"=" #'python-black-region))

;; (elpaca-use-package lsp-pyright)

