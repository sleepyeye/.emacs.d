(elpaca-use-package vertico
  :demand t
  :init
  (vertico-mode)
  (setq vertico-count 20
	vertico-cycle t))

(elpaca-use-package consult
  :demand t
  :general
  (sleepy/buffer-keys
    "d" 'kill-current-buffer
    "r" 'revert-buffer
    "b" 'consult-buffer
    "B" 'consult-buffer-other-window
    "p" 'consult-project-buffer)

  (sleepy/search-keys
    "s" 'consult-line
    "S" 'consult-line-multi
    "d" 'consult-ripgrep
    "g" 'consult-git-grep)

  (sleepy/jump-keys
    "i" 'consult-outline
    "l" 'consult-goto-line
    "o" 'consult-outline
    "d" 'consult-ripgrep
    "g" 'consult-git-grep)

  (sleepy/util-keys
    "y" 'consult-yank-pop)

  (sleepy/file-keys
    "r" 'consult-recent-file)

  :config
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

(elpaca-use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-ignore-case t)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(elpaca-use-package marginalia
  :demand t
  :init
  (setq marginalia-annotators-heavy t)
  (marginalia-mode))

(elpaca-use-package (corfu :files (:defaults "extensions/*"))
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 2)
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

