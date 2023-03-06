(use-package vertico
	:demand t
	:init
	(vertico-mode)
	(setq vertico-count 10
				vertico-cycle t))

(use-package consult
  :demand t
  :config
  (sleepy/buffer-keys
    "d" 'kill-current-buffer
    "r" 'revert-buffer
    "b" 'consult-buffer
    "B" 'consult-buffer-other-window
    "p" 'consult-project-buffer)

  ;; TODO fix name
  (defun consult-ripgrep-current ()
    (interactive)
    (consult-ripgrep default-directory))

  (sleepy/search-keys
    "i" 'consult-imenu
    "b" 'consult-line
    "B" 'consult-line-multi
    "d" 'consult-ripgrep-current
    "p" 'consult-ripgrep
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

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-ignore-case t)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :demand t
  :init
  (setq marginalia-annotators-heavy t)
  (marginalia-mode))

(use-package corfu
  :elpaca (corfu :host github :repo "minad/corfu" :files (:defaults "extensions/*"))
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5)
	(corfu-max-width 50)
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

;; (use-package cape
;;   :init
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;; 	;; FIXME set these cape-XXX as mode local
;;   ;; (add-to-list 'completion-at-point-functions #'cape-file)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-tex)
;; 	)

