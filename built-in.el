(use-package compile
  :ensure nil
  :init
  (setq compilation-scroll-output t))

(use-package autorevert
  :ensure nil
  :init
  (setq auto-revert-interval 0.01)
  (setq global-auto-revert-mode t))
