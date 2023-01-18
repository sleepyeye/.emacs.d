(elpaca-use-package magit
  :demand t
  :general
  (sleepy/util-keys
   "g" 'magit-status)
  )


(elpaca-use-package git-gutter
  :demand t
  :config
  (global-git-gutter-mode +1))


(elpaca-use-package git-timemachine)
