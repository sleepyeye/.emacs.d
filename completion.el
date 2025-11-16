;;; completion.el --- minibuffer & in-buffer completion stack -*- lexical-binding: t; -*-

(use-package vertico
  :ensure (vertico :host github :repo "minad/vertico" :files (:defaults "extensions/*"))
  :init
  (setq vertico-count 12
        vertico-cycle t)
  (vertico-mode 1)

  :config
  ;; Enable vertico-multiform for per-command/category UI customization
  (vertico-multiform-mode 1)

  ;; Configure specific commands/categories
  (setq vertico-multiform-commands
        '((consult-line buffer)
          (consult-imenu reverse)
          (execute-extended-command flat)))

  (setq vertico-multiform-categories
        '((buffer flat)
          (imenu reverse)))

  ;; Enable useful extensions
  (require 'vertico-directory)
  (require 'vertico-repeat)
  (require 'vertico-quick)

  ;; Directory navigation improvements
  (with-eval-after-load 'vertico-directory
    (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
    (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word))

  ;; Quick selection with prefix keys
  (define-key vertico-map (kbd "M-q") #'vertico-quick-insert)
  (define-key vertico-map (kbd "C-q") #'vertico-quick-exit))

(use-package consult
  :demand t
  :init
  (setq register-preview-delay 0.1
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (defun consult-ripgrep-current ()
    (interactive)
    (consult-ripgrep default-directory))

  :config
  (dolist (re '("\\`\\*Messages\\*\\'"
                "\\`\\*Completions\\*\\'"
                "\\`\\*Welcome\\*\\'"
                "\\`.*roam\\.org\\'"
                "\\`\\(?:magit\\| \\*magit\\)"))
    (add-to-list 'consult-buffer-filter re t))

  (consult-customize consult-theme consult-recent-file :preview-key nil)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.1 any))

  (setq consult-narrow-key "<"))

(use-package orderless
  :demand t
  :init
  (setq completion-styles '(orderless basic)
        completion-ignore-case t
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :commands (marginalia-mode marginalia-cycle)
  :init
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :hook (after-init . marginalia-mode))

(use-package corfu
  :ensure (corfu :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5)
  (corfu-max-width 80)
  (corfu-on-exact-match nil)

  :bind (:map corfu-map
         ("M-SPC" . corfu-insert-separator)
         ("C-n" . corfu-next)
         ("C-p" . corfu-previous))

  :init
  (global-corfu-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)

  :config
  ;; Input-based auto-trigger: disable in comments and strings
  (defun sleepy/corfu-auto-trigger-predicate ()
    "Predicate to control when corfu auto-triggers.
Disable in comments, strings, and non-code contexts."
    (and (not (nth 8 (syntax-ppss)))  ; Not in string or comment
         (not (eq (char-before) ?\s))  ; Not after space
         (not (and (derived-mode-p 'text-mode)
                   (not (derived-mode-p 'markdown-mode))))))

  ;; Apply the predicate for smarter auto-triggering
  (setq corfu-auto-commands
        '("self-insert-command\\|electric"
          org-self-insert-command))

  ;; Enable corfu-popupinfo for documentation display
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

(use-package cape
  :init
  ;; Setup completion-at-point functions with auto-trigger transformers
  (defun sleepy/cape-setup ()
    ;; Disable auto-trigger for file completion, but keep it available manually
    (add-hook 'completion-at-point-functions
              (cape-capf-noninterruptible
               (cape-capf-properties
                #'cape-file
                :sort nil
                :exclusive 'no))
              -5 t)
    ;; Add keyword completion
    (add-hook 'completion-at-point-functions #'cape-keyword -10 t))

  :hook ((prog-mode . sleepy/cape-setup)
         (text-mode . sleepy/cape-setup))

  :config
  ;; Cape prefix keymap for manual Capf triggering
  (defvar sleepy/cape-prefix-map (make-sparse-keymap)
    "Keymap for manual Cape completion sources.")

  (define-key sleepy/cape-prefix-map (kbd "f") #'cape-file)
  (define-key sleepy/cape-prefix-map (kbd "k") #'cape-keyword)
  (define-key sleepy/cape-prefix-map (kbd "s") #'cape-symbol)
  (define-key sleepy/cape-prefix-map (kbd "l") #'cape-line)
  (define-key sleepy/cape-prefix-map (kbd "w") #'cape-dict)

  ;; Bind the prefix map to C-c p for quick access
  (global-set-key (kbd "C-c p") sleepy/cape-prefix-map)

  ;; Add leader keybindings for Cape
  (with-eval-after-load 'general
    (when (fboundp 'sleepy/leader-def)
      (sleepy/leader-def
        "ic" '(:ignore t :which-key "cape")
        "icf" '(cape-file :which-key "file")
        "ick" '(cape-keyword :which-key "keyword")
        "ics" '(cape-symbol :which-key "symbol")
        "icl" '(cape-line :which-key "line")
        "icw" '(cape-dict :which-key "dict")))))

(use-package prescient :init (setq prescient-sort-full-matches-first nil))
(use-package corfu-prescient
  :after (corfu prescient)
  :hook (corfu-mode . corfu-prescient-mode)
  :init (setq corfu-prescient-enable-filtering nil
              corfu-prescient-enable-sorting t
              corfu-prescient-override-sorting t))

;; Embark - Contextual actions on completions
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ; Act on current completion candidate
   ("C-;" . embark-dwim)        ; Do What I Mean - smart default action
   ("C-h B" . embark-bindings)) ; Show all available keybindings
  :init
  ;; Replace default prefix help with embark
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide mode line in Embark collect buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark integration with Consult
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; completion.el ends here
