;;; tags.el --- Tag system integration with Citre -*- lexical-binding: t; -*-

;;; Commentary:
;; Citre integration for tag-based navigation and completion.
;; Designed to work alongside Eglot (xref backends configured in xref.el).
;;
;; Evil Keybindings:
;;   C-]     - citre-jump (jump to tag under cursor, Vim standard)
;;   C-t     - citre-jump-back (pop from tag stack, Vim standard)
;;   g]      - citre-peek (peek at definition without jumping)
;;   gK      - citre-query-jump (manual symbol query with completing-read)
;;   visual K - citre-ace-peek (ace-style peek navigation)
;;
;; Projectile Integration:
;;   SPC p T - Create tags for current project
;;   SPC p t - Update tags for current project
;;
;; Note: xref backend priorities are configured in xref.el
;;   - Completion: Tags only (avoid duplicating LSP completions)
;;
;; Requirements:
;;   - universal-ctags (brew install universal-ctags)
;;   - readtags (comes with universal-ctags)

;;; Code:

(use-package citre
  :ensure t
  :defer t
  :init
  ;; Load citre-config in :init to set up find-file-hook early
  ;; This ensures citre-auto-enable-citre-mode runs for all file opens
  (require 'citre-config)

  ;; Completion backend (xref backends are configured in xref.el)
  ;; Use tags for completion only (avoid duplicating Eglot completions)
  (setq citre-completion-backends '(tags))

  ;; Auto-enable citre-mode for programming modes
  (setq citre-auto-enable-citre-mode-modes '(prog-mode))

  ;; Universal Ctags configuration
  ;; Adjust paths if needed (these assume ctags is in PATH)
  (setq citre-ctags-program "ctags"
        citre-readtags-program "readtags")

  ;; Prompt for language when creating tags (helpful for polyglot projects)
  (setq citre-prompt-language-for-ctags-command t)

  ;; Default tags file name
  (setq citre-tags-file-names '(".tags" "tags" ".git/tags"))

  ;; Project root indicators (aligned with Projectile)
  (setq citre-project-root-function #'projectile-project-root)

  :config
  ;; ============================================================================
  ;; Evil Integration: Vim-style tag navigation
  ;; ============================================================================

  (with-eval-after-load 'evil
    ;; Traditional Vim tag bindings
    (general-define-key
     :states 'normal
     "C-]" 'citre-jump                 ; Jump to tag (Vim standard)
     "C-t" 'citre-jump-back            ; Pop tag stack (Vim standard)
     "g]" 'citre-peek                  ; Peek without jumping
     "gK" 'citre-query-jump)           ; Query-based jump

    ;; Visual mode: ace-peek for quick multi-definition navigation
    (general-define-key
     :states 'visual
     "K" 'citre-ace-peek)

    ;; Mark citre navigation commands as jumps (integrate with jump list)
    (evil-set-command-property 'citre-jump :jump t)
    (evil-set-command-property 'citre-query-jump :jump t))

  ;; ============================================================================
  ;; Completion Integration: Conditional activation
  ;; ============================================================================

  ;; Strategy: Disable tags completion when Eglot is managing the buffer
  ;; to avoid duplicate entries in Corfu popup
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook
      (lambda ()
        ;; When LSP is active, rely solely on LSP completions
        (setq-local citre-completion-backends nil))))

  ;; Alternative strategy (commented out):
  ;; Keep tags completion even with LSP - useful if LSP is incomplete
  ;; (setq citre-completion-backends '(tags))

  ;; ============================================================================
  ;; Projectile Integration: Tag management commands
  ;; ============================================================================

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

    ;; Add to Projectile command map
    (with-eval-after-load 'general
      (when (fboundp 'sleepy/leader-def)
        (sleepy/leader-def
          "p T" '(sleepy/citre-create-tags-for-project :which-key "create tags")
          "p t" '(sleepy/citre-update-tags-for-project :which-key "update tags")))))

  ;; ============================================================================
  ;; Utility: Ensure tags are gitignored
  ;; ============================================================================

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

  ;; Optional: Auto-run when creating tags
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
    ;; Fallback to default imenu
    (imenu-default-create-index-function)))

;; Hook to set imenu function for prog-mode
(add-hook 'prog-mode-hook
  (lambda ()
    (when (fboundp 'citre-mode)
      (setq-local imenu-create-index-function #'sleepy/citre-imenu-or-default))))

;; ============================================================================
;; Optional: citre-peek window configuration
;; ============================================================================

(with-eval-after-load 'citre-peek
  ;; Peek window size and behavior
  (setq citre-peek-file-content-height 12
        citre-peek-auto-restore-after-jump t))

(provide 'tags)
;;; tags.el ends here
