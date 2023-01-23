;; backup, lock and custom files
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq display-line-numbers t)
(setq create-lockfiles nil
      auto-save-default t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


(recentf-mode 1)
(global-hl-line-mode +1)


;; FIXME
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 100))
(set-face-attribute 'default (selected-frame) :height 200)
