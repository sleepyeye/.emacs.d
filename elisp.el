(defun sleepy/elisp-capf ()
  (setq-local completion-at-point-functions
			  (list (cape-capf-super
					 ;; for emacs-lisp symbols
					 #'citre-completion-at-point
					 #'elisp-completion-at-point
					 #'cape-elisp-symbol
					 #'cape-file
					 ))))

;; elisp mode do not use eglot.
(add-hook 'emacs-lisp-mode-hook #'sleepy/elisp-capf)
