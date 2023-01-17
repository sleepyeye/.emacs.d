(elpaca-use-package osx-trash
  :commands osx-trash-move-file-to-trash
  :init
  ;; Delete files to trash on macOS, as an extra layer of precaution against
  ;; accidentally deleting wanted files.
  (setq delete-by-moving-to-trash t)

  ;; Lazy load `osx-trash'
  (when (not (fboundp 'system-move-file-to-trash))
    (defun system-move-file-to-trash (file)
      "Move FILE to trash."
      (when (and (not IS-LINUX)
                 (not (file-remote-p default-directory)))
        (osx-trash-move-file-to-trash file)))))



  
(elpaca-use-package ns-auto-titlebar)


(setq locate-command "mdfind")
(setq ns-use-native-fullscreen nil)
(setq ns-pop-up-frames nil)
(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)
(and (or (daemonp)
         (display-graphic-p))
     (require 'ns-auto-titlebar nil t)
     (ns-auto-titlebar-mode +1))



