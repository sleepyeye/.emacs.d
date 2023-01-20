(elpaca-use-package perspective
  :demand t
  :after consult
  :general
  (sleepy/workspace-keys
    "TAB" #'persp-switch
    "[" #'persp-prev
    "]" #'persp-next
    "s" #'persp-save
    "l" #'persp-load
    "1" #'(lambda () (interactive) (persp-switch "main"))
    "2" #'(lambda () (interactive) (persp-switch "2"))
    "3" #'(lambda () (interactive) (persp-switch "3"))
    "4" #'(lambda () (interactive) (persp-switch "4"))
    "5" #'(lambda () (interactive) (persp-switch "5"))
    "6" #'(lambda () (interactive) (persp-switch "6"))
    "7" #'(lambda () (interactive) (persp-switch "7"))
    "8" #'(lambda () (interactive) (persp-switch "8"))
    "9" #'(lambda () (interactive) (persp-switch "9"))
    "d" #'persp-kill-buffer*
    "D" #'persp-kill-others)


  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  :init
  (persp-mode))


