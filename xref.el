;;; xref.el --- xref backend configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralized xref backend configuration for navigation.
;;
;; Strategy:
;;   - Global default: dumb-jump (fast fallback for any file type)
;;   - Per-mode backends prepended via hooks for optimal ordering
;;   - Eglot auto-prepends its backend when active (via eglot-extend-to-xref t)
;;
;; Backend Priority Order (first match wins):
;;   Python/C/C++/LaTeX: eglot > citre > dumb-jump
;;   Emacs Lisp:         elisp > citre > dumb-jump
;;   Shell:              citre > dumb-jump
;;   Other:              dumb-jump
;;
;; Note: eglot-xref-backend is automatically prepended when eglot is active,
;; so we only need to set up citre and elisp backends in mode hooks.

;;; Code:

;; Global default: dumb-jump as universal fallback
;; This provides basic regex-based navigation for any file type
(setq-default xref-backend-functions '(dumb-jump-xref-activate))

;; Xref Settings
;; Use ripgrep for faster project-wide searches
(setq xref-search-program 'ripgrep)

;; Per-window navigation history (Emacs 29+)
(when (boundp 'xref-history-storage)
  (setq xref-history-storage 'xref-window-local-history))

;; Helper macro to set up xref backends for a mode
(defmacro sleepy/setup-xref-backends (mode &rest backends)
  "Set up xref backends for MODE.
BACKENDS are added in order (first backend has highest priority).
The global default (dumb-jump) is appended automatically.
Note: eglot-xref-backend is auto-prepended when eglot is active."
  (declare (indent 1))
  `(add-hook ',(intern (concat (symbol-name mode) "-hook"))
     (lambda ()
       (setq-local xref-backend-functions
                   (append ',backends
                           (default-value 'xref-backend-functions))))))

;; ============================================================================
;; Per-Mode Backend Configuration
;; ============================================================================

;; Python: eglot (auto) > citre > dumb-jump
;; eglot-xref-backend is prepended automatically when eglot is active
(sleepy/setup-xref-backends python-mode
  citre-xref-backend)

(sleepy/setup-xref-backends python-ts-mode
  citre-xref-backend)

;; C/C++: eglot (auto) > citre > dumb-jump
(sleepy/setup-xref-backends c-mode
  citre-xref-backend)

(sleepy/setup-xref-backends c++-mode
  citre-xref-backend)

(sleepy/setup-xref-backends c-ts-mode
  citre-xref-backend)

(sleepy/setup-xref-backends c++-ts-mode
  citre-xref-backend)

;; Emacs Lisp: elisp > citre > dumb-jump
;; elisp--xref-backend is the built-in elisp navigation
(sleepy/setup-xref-backends emacs-lisp-mode
  elisp--xref-backend
  citre-xref-backend)

;; LaTeX: eglot (auto) > citre > dumb-jump
(sleepy/setup-xref-backends LaTeX-mode
  citre-xref-backend)

;; Shell: citre > dumb-jump (no LSP typically)
(sleepy/setup-xref-backends sh-mode
  citre-xref-backend)

(sleepy/setup-xref-backends bash-ts-mode
  citre-xref-backend)

(provide 'xref-config)
;;; xref.el ends here
