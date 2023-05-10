(defun sleepy/python-capf ()
  (setq-local completion-at-point-functions
			  (list (cape-super-capf
					 #'tempel-expand
					 #'eglot-completion-at-point
					 #'cape-keyword
					 #'cape-dabbrev
					 ))))


(with-eval-after-load 'python-mode
  (add-hook 'eglot-managed-mode-hook #'sleepy/python-capf))

(with-eval-after-load 'python-ts-mode
  (add-hook 'eglot-managed-mode-hook #'sleepy/python-capf))

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
