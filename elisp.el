;;; elisp.el --- Emacs Lisp development configuration -*- lexical-binding: t; -*-

(defun sleepy/elisp-capf ()
  "Setup completion-at-point for Emacs Lisp mode."
  (setq-local completion-at-point-functions
			  (list (cape-capf-super
					 ;; for emacs-lisp symbols
					 #'cape-elisp-symbol
					 #'cape-file))))

(defun sleepy/elisp-setup ()
  "Setup Emacs Lisp mode configuration."
  (sleepy/elisp-capf)
  ;; Reset imenu to use Emacs Lisp's default imenu implementation
  ;; (overriding citre-imenu from tags.el prog-mode-hook)
  (setq-local imenu-create-index-function nil)
  ;; Ensure imenu auto-rescans
  (setq-local imenu-auto-rescan t)
  ;; Disable dumb-jump for elisp - use built-in elisp-xref instead
  ;; This prevents freezing when using gd/gr in elisp files
  (remove-hook 'xref-backend-functions #'dumb-jump-xref-activate t))

;; elisp mode does not use eglot.
(add-hook 'emacs-lisp-mode-hook #'sleepy/elisp-setup)

;;; elisp.el ends here
