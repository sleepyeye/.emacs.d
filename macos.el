;;; macos.el --- macOS-specific configuration -*- lexical-binding: t; -*-

;; Move deleted files to trash on macOS
(use-package osx-trash
  :ensure t
  :commands osx-trash-move-file-to-trash
  :init
  ;; Override system trash function with osx-trash
  (when (not (fboundp 'system-move-file-to-trash))
    (defun system-move-file-to-trash (file)
      "Move FILE to trash using osx-trash."
      (when (not (file-remote-p default-directory))
        (osx-trash-move-file-to-trash file)))))

;; Dark mode aware titlebar
(use-package ns-auto-titlebar
  :ensure t
  :config
  (when (or (daemonp) (display-graphic-p))
    (ns-auto-titlebar-mode +1)))

;; macOS-specific settings
(setq locate-command "mdfind"                    ; Use Spotlight for locate
      ns-use-native-fullscreen nil               ; Don't use native fullscreen
      ns-pop-up-frames nil                       ; Don't create new frames
      mac-redisplay-dont-reset-vscroll t         ; Keep scroll position
      mac-mouse-wheel-smooth-scroll nil)         ; Disable smooth scrolling

;;; macos.el ends here
