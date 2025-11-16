;;; completion.el --- minibuffer & in-buffer completion stack -*- lexical-binding: t; -*-

(use-package vertico
  :init
  (setq vertico-count 12
        vertico-cycle t)
  (vertico-mode 1))

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
         ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode 1)
  (corfu-history-mode 1))

(use-package cape
  :init
  (defun sleepy/cape-setup ()
    (add-hook 'completion-at-point-functions #'cape-file -5 t)
    (add-hook 'completion-at-point-functions #'cape-keyword -10 t))
  :hook ((prog-mode . sleepy/cape-setup)
         (text-mode . sleepy/cape-setup)))

(use-package prescient :init (setq prescient-sort-full-matches-first nil))
(use-package corfu-prescient
  :after (corfu prescient)
  :hook (corfu-mode . corfu-prescient-mode)
  :init (setq corfu-prescient-enable-filtering nil
              corfu-prescient-enable-sorting t
              corfu-prescient-override-sorting t))

;;; completion.el ends here
