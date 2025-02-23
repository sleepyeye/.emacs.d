(defun sleepy/elisp-capf ()
  (setq-local completion-at-point-functions
			  (list (cape-capf-super
					 #'tempel-complete
					 ;; for emacs-lisp symbols
					 #'cape-elisp-symbol
					 #'cape-file))))


;; elisp mode do not use eglot.
(add-hook 'emacs-lisp-mode-hook #'sleepy/elisp-capf)
