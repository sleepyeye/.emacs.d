(defun sleepy/elisp-capf ()
  (setq-local completion-at-point-functions
			  (list (cape-super-capf
					 #'tempel-complete
					 ;; for emacs-lisp symbols
					 #'elisp-completion-at-point
					 #'cape-symbol
					 #'cape-file
					 ))))

(with-eval-after-load 'emacs-lisp-mode
  (add-hook 'eglot-managed-mode-hook #'sleepy/elisp-capf))
