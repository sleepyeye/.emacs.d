(use-package project :ensure nil)

(use-package projectile
  :demand t
  :general
  :config
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "^\\build$")
  (add-to-list 'projectile-globally-ignored-directories "^\\elpa$")
  (add-to-list 'projectile-globally-ignored-directories "^\\url$")
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root t))


(use-package consult-projectile)
