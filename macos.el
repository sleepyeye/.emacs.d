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
  :hook (after-init . ns-auto-titlebar-mode))

;; macOS-specific settings (deferred for startup performance)
(add-hook 'after-init-hook
          (lambda ()
            (setq locate-command "mdfind"                    ; Use Spotlight for locate
                  ns-use-native-fullscreen nil               ; Don't use native fullscreen
                  ns-pop-up-frames nil                       ; Don't create new frames
                  mac-redisplay-dont-reset-vscroll t         ; Keep scroll position
                  mac-mouse-wheel-smooth-scroll nil          ; Disable smooth scrolling
                  ;; Korean input method - Use macOS native input
                  default-input-method nil                   ; Use macOS input instead of Emacs
                  mac-pass-command-to-system t)))            ; Let macOS handle Cmd key for input switching

;; Optional: If you prefer Emacs builtin Korean input, uncomment:
;; (setq default-input-method "korean-hangul")   ; 2-bul (두벌식)
;; (global-set-key (kbd "C-;") 'toggle-input-method)

;;; macos.el ends here
