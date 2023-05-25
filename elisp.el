(defun sleepy/elisp-capf ()
  (setq-local completion-at-point-functions
			  (list (cape-super-capf
					 #'tempel-complete
					 ;; for emacs-lisp symbols
					 #'elisp-completion-at-point
					 #'cape-symbol
					 #'cape-file
					 ))))

(add-hook 'emacs-lisp-mode-hook #'sleepy/elisp-capf))
