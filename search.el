;;; search.el --- ripgrep / dumb-jump setup -*- lexical-binding: t; -*-

;; dumb-jump (use as xref backend)
(use-package dumb-jump
  :commands dumb-jump-xref-activate
  :init
  ;; Prefer ripgrep (faster)
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-force-searcher  'rg)
  :config
  ;; Register as xref backend → integrates with default xref UI/keys
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;;; --- Imenu: only classes & functions ---------------------------------

;; Show hierarchy in one line in consult-imenu (e.g., Class.method)
(with-eval-after-load 'consult
  (setq consult-imenu-namespace 'concat))

;; Common: auto-rescan and accuracy options
(setq imenu-auto-rescan t
	  imenu-use-markers t)

;;; search.el ends here
