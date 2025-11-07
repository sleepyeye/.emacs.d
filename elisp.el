;;; elisp.el --- Emacs Lisp development configuration -*- lexical-binding: t; -*-

(defun sleepy/elisp-capf ()
  "Setup completion-at-point for Emacs Lisp mode."
  (setq-local completion-at-point-functions
			  (list (cape-capf-super
					 ;; for emacs-lisp symbols
					 #'cape-elisp-symbol
					 #'cape-file))))

;; elisp mode does not use eglot.
(add-hook 'emacs-lisp-mode-hook #'sleepy/elisp-capf)

;;; elisp.el ends here
