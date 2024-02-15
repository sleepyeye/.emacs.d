(use-package perspective
  :demand t
  :after consult
  :init
  (setq persp-state-default-file (expand-file-name ".persp" user-emacs-directory))
  (setq persp-suppress-no-prefix-key-warning t)
  :config
  (sleepy/leader-def
    "TAB TAB" #'persp-switch
    "TAB `" 'persp-switch-last
    "TAB d" 'persp-kill
    "TAB h" 'persp-prev
    "TAB l" 'persp-next
    "TAB x" '((lambda () (interactive) (persp-kill (persp-current-name))) :wk "kill current")
    "TAB X" '((lambda () (interactive) (persp-kill (persp-names))) :wk "kill all")
    "TAB [" #'persp-prev
    "TAB ]" #'persp-next
    "TAB s" #'persp-save
    "TAB l" #'persp-load
    "TAB 1" #'(lambda () (interactive) (persp-switch "main"))
    "TAB 2" #'(lambda () (interactive) (persp-switch "2"))
    "TAB 3" #'(lambda () (interactive) (persp-switch "3"))
    "TAB 4" #'(lambda () (interactive) (persp-switch "4"))
    "TAB 5" #'(lambda () (interactive) (persp-switch "5"))
    "TAB 6" #'(lambda () (interactive) (persp-switch "6"))
    "TAB 7" #'(lambda () (interactive) (persp-switch "7"))
    "TAB 8" #'(lambda () (interactive) (persp-switch "8"))
    "TAB 9" #'(lambda () (interactive) (persp-switch "9"))
    "TAB d" #'persp-kill-buffer*
    "TAB D" #'persp-kill-others)

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
