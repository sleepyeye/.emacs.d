(use-package python
  :after projectile 
  :init
  (setq python-indent-guess-indent-offset-verbose nil
	flycheck-flake8-maximum-line-length 120)
  :config
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
      (setq python-shell-interpreter "python3"))
  ))



