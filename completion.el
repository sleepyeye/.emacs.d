(use-package vertico
  :defer t
  :init
  (vertico-mode)
  (setq vertico-count 12
		vertico-cycle t))


(use-package consult
  :demand t
  :config
  (add-to-list 'consult-buffer-filter ".*roam.org$")
  (add-to-list 'consult-buffer-filter "\\`\\*Messages\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Completions\\*\\'")
  (add-to-list 'consult-buffer-filter "\\`\\*Welcome\\*\\'")
  (add-to-list 'consult-buffer-filter "magit*")

  ;; TODO fix name
  (defun consult-ripgrep-current ()
	(interactive)
	(consult-ripgrep default-directory))
  :config
  ;; Turn off live-preview
  (consult-customize consult-theme consult-recent-file :preview-key nil)
   ;; Give some live-preview delays for these functions
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.1 any))
  (setq consult-narrow-key "<")

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref))

(use-package orderless
  :demand t
  :init
  (setq completion-styles '(orderless basic)
		completion-ignore-case t
		completion-category-defaults nil
		completion-category-overrides '((file (styles partial-completion)))))


(use-package marginalia
  :hook (after-init . marginalia-mode)
  :commands (marginalia-mode marginalia-cycle)
  :init
  (setq marginalia-annotators-heavy t))

(use-package corfu
  :ensure (corfu :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  :defer t
  :commands (corfu-mode global-corfu-mode)
  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5)
  (corfu-max-width 80)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-history-mode 1))

(use-package cape
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword))

(use-package prescient
  :ensure t
  :init
  (setq prescient-sort-full-matches-first nil))

(use-package corfu-prescient
  :ensure t
  :after (corfu prescient)
  :hook (corfu-mode . corfu-prescient-mode)
  :init
  (setq corfu-prescient-enable-filtering t
		corfu-prescient-override-sorting t
		corfu-prescient-enable-sorting t))
