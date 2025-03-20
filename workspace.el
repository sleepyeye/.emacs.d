(use-package perspective
  :demand t
  :after consult
  :init
  (setq persp-state-default-file (expand-file-name ".persp" user-emacs-directory))
  (setq persp-suppress-no-prefix-key-warning t)
  :config

  ;;; see https://github.com/minad/consult/wiki#perspective
  (consult-customize consult--source-buffer :hidden t :default nil)
  (defvar consult--source-perspective
	(list :name     "Perspective"
          :narrow   ?s
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    #'persp-get-buffer-names))
  (push consult--source-perspective consult-buffer-sources)

  :init
  (persp-mode)
  (add-hook 'kill-emacs-hook #'persp-state-save))
