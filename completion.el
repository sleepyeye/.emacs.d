(use-package vertico
  :demand t
  :init
  (vertico-mode)
  (setq vertico-count 10
		vertico-cycle t))

(use-package consult
  :demand t
  :config
  (sleepy/leader-def
	"bd" 'kill-current-buffer
	"br" 'revert-buffer
	"bb" 'consult-buffer
	"bB" 'consult-buffer-other-window
	"bp" 'consult-project-buffer)

  ;; TODO fix name
  (defun consult-ripgrep-current ()
	(interactive)
	(consult-ripgrep default-directory))

  (sleepy/leader-def
	"si" 'consult-imenu
	"sb" 'consult-line
	"sB" 'consult-line-multi
	"sd" 'consult-ripgrep-current
	"sp" 'consult-ripgrep
	"sg" 'consult-git-grep)

  (sleepy/leader-def
	"ji" 'consult-outline
	"jl" 'consult-goto-line
	"jo" 'consult-outline
	"jd" 'consult-ripgrep
	"jg" 'consult-git-grep)

  (sleepy/leader-def
	"uy" 'consult-yank-pop)

  (sleepy/leader-def
	"fr" 'consult-recent-file)

  :config
  ;; Turn off live-preview
  (consult-customize
   consult-theme
   consult-recent-file
   :preview-key nil)


  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-ignore-case t)
  (completion-category-defaults nil)
  ;; (completion-category-overrides '((eglot (styles orderless))
  ;; 								   (file (styles basic partial-completion))))
  )



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
  (corfu-history-mode 1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; FIXME set these cape-XXX as mode local
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  )

