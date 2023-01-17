(setq package-enable-at-startup nil)
(setq inhibit-default-init nil)
(setq native-comp-async-report-warnings-errors nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)



;; Set Garbage Collection threshold to 1GB during startup. `gcmh' will clean
;; things up later.
(setq gc-cons-threshold 1073741824
      gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024 32)) ;; 32mb


;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)


;; add custom exec path
(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'exec-path "/Library/TeX/texbin")


(provide 'early-init)
