;;; code-navigation.el --- xref, tags, and LSP navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified navigation module that consolidates:
;; - Xref backend configuration
;; - Citre tags integration
;; - Eglot LSP configuration

;;; Code:

;; ============================================================================
;; Xref backend configuration
;; ============================================================================

;; Global default: dumb-jump as universal fallback
;; This provides basic regex-based navigation for any file type.
(setq-default xref-backend-functions '(dumb-jump-xref-activate))

;; Use ripgrep for faster project-wide searches.
(setq xref-search-program 'ripgrep)

;; Per-window navigation history (Emacs 29+).
(when (boundp 'xref-history-storage)
  (setq xref-history-storage 'xref-window-local-history))

(defun sleepy/citre-xref-backend ()
  "Call citre-xref-backend if available and tags file exists."
  (when (and (fboundp 'citre-xref-backend)
             (fboundp 'citre-tags-file-path)
             (citre-tags-file-path))
    (citre-xref-backend)))

(defun sleepy--normalize-xref-backend-entry (backend)
  "Normalize xref BACKEND symbol to local wrapper."
  (if (eq backend 'citre-xref-backend)
      'sleepy/citre-xref-backend
    backend))

(defun sleepy--normalize-xref-backends ()
  "Normalize and deduplicate `xref-backend-functions' in current buffer."
  (let* ((preferred
          (when (derived-mode-p 'emacs-lisp-mode)
            '(elisp--xref-backend
              sleepy/citre-xref-backend
              dumb-jump-xref-activate)))
         (combined
          (append preferred
                  xref-backend-functions
                  (default-value 'xref-backend-functions)))
         normalized)
    (dolist (backend combined)
      (setq backend (sleepy--normalize-xref-backend-entry backend))
      (unless (member backend normalized)
        (setq normalized (append normalized (list backend)))))
    (setq-local xref-backend-functions normalized)))

(defmacro sleepy/setup-xref-backends (mode &rest backends)
  "Set up xref backends for MODE.
BACKENDS are added in order (first backend has highest priority).
The global default (dumb-jump) is appended automatically.
Note: eglot-xref-backend is auto-prepended when Eglot is active."
  (declare (indent 1))
  `(add-hook ',(intern (concat (symbol-name mode) "-hook"))
     (lambda ()
       (setq-local xref-backend-functions
                   (append ',backends
                           (default-value 'xref-backend-functions)))
       (sleepy--normalize-xref-backends))))

;; Python: eglot (auto) > citre > dumb-jump
(sleepy/setup-xref-backends python-mode
  sleepy/citre-xref-backend)

(sleepy/setup-xref-backends python-ts-mode
  sleepy/citre-xref-backend)

;; C/C++: eglot (auto) > citre > dumb-jump
(sleepy/setup-xref-backends c-mode
  sleepy/citre-xref-backend)

(sleepy/setup-xref-backends c++-mode
  sleepy/citre-xref-backend)

(sleepy/setup-xref-backends c-ts-mode
  sleepy/citre-xref-backend)

(sleepy/setup-xref-backends c++-ts-mode
  sleepy/citre-xref-backend)

;; Emacs Lisp: elisp > citre > dumb-jump
(sleepy/setup-xref-backends emacs-lisp-mode
  elisp--xref-backend
  sleepy/citre-xref-backend)

;; LaTeX: eglot (auto) > citre > dumb-jump
(sleepy/setup-xref-backends LaTeX-mode
  sleepy/citre-xref-backend)

;; Shell: citre > dumb-jump
(sleepy/setup-xref-backends sh-mode
  sleepy/citre-xref-backend)

(sleepy/setup-xref-backends bash-ts-mode
  sleepy/citre-xref-backend)

;; ============================================================================
;; Citre tags integration
;; ============================================================================

(use-package citre
  :ensure t
  :defer t
  :init
  ;; Use tags for completion only to avoid duplicating Eglot completions.
  (setq citre-completion-backends '(tags))
  (setq citre-auto-enable-citre-mode-modes '(prog-mode))

  ;; Universal Ctags configuration.
  (setq citre-ctags-program "ctags"
        citre-readtags-program "readtags")
  (setq citre-prompt-language-for-ctags-command t)
  (setq citre-tags-file-names '(".tags" "tags" ".git/tags"))
  (setq citre-project-root-function #'projectile-project-root)

  :config
  ;; Load citre-config after package activation to avoid first-boot failures.
  (require 'citre-config)

  ;; Evil-style tag navigation.
  (with-eval-after-load 'evil
    (general-define-key
     :states 'normal
     "C-]" 'citre-jump
     "C-t" 'citre-jump-back
     "g]" 'citre-peek
     "gK" 'citre-query-jump)
    (general-define-key
     :states 'visual
     "K" 'citre-ace-peek)
    (evil-set-command-property 'citre-jump :jump t)
    (evil-set-command-property 'citre-query-jump :jump t))

  ;; Disable tags completion in Eglot-managed buffers.
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (setq-local citre-completion-backends nil))))

  ;; Citre may prepend its backend directly; normalize chain after mode toggles.
  (add-hook 'citre-mode-hook #'sleepy--normalize-xref-backends)

  ;; Projectile integration for project tag maintenance.
  (with-eval-after-load 'projectile
    (defun sleepy/citre-create-tags-for-project ()
      "Create tags file for current Projectile project."
      (interactive)
      (if-let ((root (projectile-project-root)))
          (let ((default-directory root))
            (call-interactively 'citre-create-tags-file)
            (message "Tags file created for project: %s" (projectile-project-name)))
        (user-error "Not in a Projectile project")))

    (defun sleepy/citre-update-tags-for-project ()
      "Update tags file for current Projectile project."
      (interactive)
      (if-let ((root (projectile-project-root)))
          (let ((default-directory root))
            (if (citre-tags-file-path)
                (progn
                  (citre-update-this-tags-file)
                  (message "Tags updated for project: %s" (projectile-project-name)))
              (if (y-or-n-p "No tags file found. Create one? ")
                  (sleepy/citre-create-tags-for-project)
                (user-error "Aborted"))))
        (user-error "Not in a Projectile project")))

    (with-eval-after-load 'general
      (when (fboundp 'sleepy/leader-def)
        (sleepy/leader-def
          "p T" '(sleepy/citre-create-tags-for-project :which-key "create tags")
          "p t" '(sleepy/citre-update-tags-for-project :which-key "update tags")))))

  (defun sleepy/ensure-tags-gitignored ()
    "Add common tag file patterns to .gitignore if not present."
    (interactive)
    (when-let ((root (and (fboundp 'projectile-project-root)
                          (projectile-project-root))))
      (let ((gitignore (expand-file-name ".gitignore" root)))
        (if (file-exists-p gitignore)
            (with-temp-buffer
              (insert-file-contents gitignore)
              (unless (or (save-excursion (search-forward "tags" nil t))
                          (save-excursion (search-forward "TAGS" nil t))
                          (save-excursion (search-forward ".tags" nil t)))
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert "\n# Tag files\ntags\nTAGS\n.tags\n")
                (write-file gitignore)
                (message "Added tag patterns to .gitignore")))
          (when (y-or-n-p "No .gitignore found. Create one with tag patterns? ")
            (with-temp-buffer
              (insert "# Tag files\ntags\nTAGS\n.tags\n")
              (write-file gitignore)
              (message "Created .gitignore with tag patterns")))))))

  (advice-add 'citre-create-tags-file :after
              (lambda (&rest _)
                (when (and (fboundp 'projectile-project-root)
                           (projectile-project-root))
                  (sleepy/ensure-tags-gitignored)))))

(defun sleepy/citre-imenu-or-default ()
  "Use citre-imenu if tags file exists, otherwise use default imenu."
  (if (and (fboundp 'citre-tags-file-path)
           (citre-tags-file-path))
      (citre-imenu-create-index-function)
    (imenu-default-create-index-function)))

(add-hook 'prog-mode-hook
          (lambda ()
            (when (fboundp 'citre-mode)
              (setq-local imenu-create-index-function #'sleepy/citre-imenu-or-default))))

(with-eval-after-load 'citre-peek
  (setq citre-peek-file-content-height 12
        citre-peek-auto-restore-after-jump t))

;; ============================================================================
;; Eglot LSP configuration
;; ============================================================================

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook
  ((python-mode python-ts-mode) . eglot-ensure)
  ((c-mode c++-mode c-ts-mode c++-ts-mode) . eglot-ensure)
  (LaTeX-mode . eglot-ensure)
  ((cmake-mode cmake-ts-mode) . eglot-ensure)
  :custom
  (eglot-report-progress nil)
  (eglot-events-buffer-size 0)
  :init
  (setq eglot-autoshutdown t
        eglot-sync-connect 0)

  (defun sleepy/eglot-workspace-config (_server)
    "Return workspace configuration based on major mode."
    (cond
     ((derived-mode-p 'python-base-mode)
      '(:basedpyright (:typeCheckingMode "standard"
                       :disableOrganizeImports t)
        :basedpyright.analysis (:inlayHints (:callArgumentNames :json-false)
                               :diagnosticSeverityOverrides
                               (:reportCallIssue "none"
                                :reportUnusedCallResult "none"
                                :reportGeneralTypeIssues "none"
                                :reportArgumentType "none")
                               :useLibraryCodeForTypes t
                               :autoImportCompletions :json-false
                               :stubPath ["./" "./typings"]
                               :diagnosticMode "openFilesOnly"
                               :autoSearchPaths t)))
     ((derived-mode-p 'latex-mode 'LaTeX-mode)
      '(:texlab (:completion (:matcher "fuzzy"))))
     (t nil)))

  (setq eglot-workspace-configuration #'sleepy/eglot-workspace-config)

  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq jsonrpc-event-hook nil)
  (setq eglot-send-changes-idle-time 0.5)
  (setq eglot-extend-to-xref t
        eglot-ignored-server-capabilities '(:inlayHintProvider :foldingRangeProvider))

  ;; Update only Eglot category to avoid clobbering global completion settings.
  (setf (alist-get 'eglot completion-category-overrides)
        '(styles orderless basic))

  (defun sleepy/cape-wrap-buster-maybe (orig-fun &rest args)
    "Apply cape-wrap-buster except in LaTeX mode."
    (if (derived-mode-p 'latex-mode 'LaTeX-mode)
        (apply orig-fun args)
      (cape-wrap-buster (apply orig-fun args))))
  (advice-add 'eglot-completion-at-point :around #'sleepy/cape-wrap-buster-maybe)

  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ("clangd" "-j=2" "--log=error" "--completion-style=bundled"
                    "--background-index" "--header-insertion=never"
                    "--header-insertion-decorators=0")))
  (add-to-list 'eglot-server-programs '((LaTeX-mode) . ("texlab")))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("basedpyright-langserver" "--stdio"))))

(use-package consult-eglot
  :after eglot
  :config
  (when (fboundp 'sleepy/leader-def)
    (sleepy/leader-def "cs" #'consult-eglot-symbols)))

(with-eval-after-load 'eglot
  (with-eval-after-load 'general
    (when (fboundp 'sleepy/leader-def)
      (sleepy/leader-def
        "c a" '(eglot-code-actions :which-key "code actions")
        "c r" '(eglot-rename :which-key "rename symbol")
        "c f" '(eglot-format :which-key "format buffer/region")
        "c o" '(eglot-code-action-organize-imports :which-key "organize imports")
        "c d" '(xref-find-definitions :which-key "find definitions")
        "c D" '(xref-find-references :which-key "find references")
        "c i" '(eglot-find-implementation :which-key "find implementation")
        "c t" '(eglot-find-typeDefinition :which-key "find type definition")
        "c h" '(eldoc-doc-buffer :which-key "show documentation")
        "c R" '(eglot-reconnect :which-key "restart LSP server")
        "c S" '(eglot-shutdown :which-key "shutdown LSP server")))))

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode 1))

;; Provide compatibility symbols for old split modules.
(provide 'code-navigation)
(provide 'xref-config)
(provide 'tags)

;;; code-navigation.el ends here
