;;; early-init.el --- early-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;; --- Startup Performance Optimizations (restore after init) ----------------
;; Temporarily disable GC during startup for faster boot
(defvar sleepy--initial-gc-cons-threshold gc-cons-threshold
  "Store initial GC threshold to restore later.")
(defvar sleepy--initial-gc-cons-percentage gc-cons-percentage
  "Store initial GC percentage to restore later.")
(defvar sleepy--initial-file-name-handler-alist file-name-handler-alist
  "Store initial file-name-handler-alist to restore later.")

;; GC configuration constants
(defconst sleepy/gc-cons-threshold-mb 80
  "Normal GC threshold in megabytes.
Used after startup completes. During startup, GC is effectively disabled
for maximum performance.")

(defconst sleepy/gc-cons-percentage-normal 0.1
  "Normal GC percentage of heap size.
Lower values mean more frequent but shorter GC pauses.")

(defconst sleepy/gc-cons-percentage-startup 0.6
  "GC percentage during startup.
Higher value defers GC until more memory is allocated.")

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage sleepy/gc-cons-percentage-startup
      file-name-handler-alist nil)

;; Restore GC settings and file-name-handler-alist after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* sleepy/gc-cons-threshold-mb 1024 1024)
                  gc-cons-percentage sleepy/gc-cons-percentage-normal
                  file-name-handler-alist sleepy--initial-file-name-handler-alist)))

;; Basic Startup Settings
(setq package-enable-at-startup nil
      native-comp-async-report-warnings-errors nil
      ring-bell-function 'ignore
      inhibit-startup-screen t
      inhibit-default-init nil
      initial-buffer-choice nil
      inhibit-startup-buffer-menu t
      inhibit-x-resources t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      auto-mode-case-fold nil
      frame-inhibit-implied-resize t)

;; Performance and Optimization
(defconst sleepy/idle-update-delay 0.3
  "Delay in seconds before updating idle timers.
Higher values reduce CPU usage but may feel less responsive.")

(defconst sleepy/read-process-output-max-mb 4
  "Maximum bytes to read from subprocess in one chunk, in megabytes.
Higher values improve LSP performance but use more memory.")

(setq idle-update-delay sleepy/idle-update-delay
      bidi-display-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil
      fast-but-imprecise-scrolling t
      inhibit-compacting-font-caches t
      load-prefer-newer t
      read-process-output-max (* sleepy/read-process-output-max-mb 1024 1024))

;; UI Tweaks
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq frame-resize-pixelwise t)

;; Frame Size
(defconst sleepy/default-frame-height 60
  "Default frame height in lines.")

(defconst sleepy/default-frame-width 100
  "Default frame width in columns.")

(add-to-list 'default-frame-alist (cons 'height sleepy/default-frame-height))
(add-to-list 'default-frame-alist (cons 'width sleepy/default-frame-width))

;; System Type Detection
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

;; PATH and exec-path Setup
(defun sleepy--add-paths-to-env (paths)
  "Add PATHS to both PATH environment variable and exec-path.
PATHS should be a list of directory path strings.
Paths are expanded and prepended to the existing PATH."
  (let ((path-string (mapconcat (lambda (p) (expand-file-name p))
                                paths
                                path-separator)))
    (setenv "PATH" (concat path-string path-separator (getenv "PATH")))
    (dolist (path paths)
      (add-to-list 'exec-path (expand-file-name path)))))

(cond
 (IS-MAC
  (sleepy--add-paths-to-env
   '("/opt/homebrew/bin"
     "~/.cargo/bin"
     "/Library/TeX/texbin"
     "~/miniforge3/bin"
     "~/.local/bin"
     "/usr/local/bin")))

 (IS-LINUX
  (sleepy--add-paths-to-env
   '("~/.local/bin"
     "~/.cargo/bin"
     "~/miniconda3/bin"
     "/usr/local/bin"
     "/usr/local/sbin"
     "/usr/sbin"
     "/sbin"))))

;; macOS-specific Frame Appearance
(when IS-MAC
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Disable Startup Messages
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;; Disable x-session resource loading
(advice-add #'x-apply-session-resources :override #'ignore)

;; Load elpaca and wait
(defvar user-emacs-directory "~/.emacs.d")
(load (concat user-emacs-directory "bootstraps"))
(defvar local-package-directory (expand-file-name "~/.emacs.d/local-packages/"))
(elpaca-wait)

;; GCMH - Garbage Collector Magic Hack
;; Run GC only when Emacs is idle to avoid pauses during typing
(use-package gcmh
  :ensure t
  :demand t
  :config
  (setq gcmh-idle-delay 'auto       ; GC after idle time (auto = 15s)
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 128 1024 1024)) ; 128MB
  (gcmh-mode 1))

;; evil keybinding setup for elpaca
(with-eval-after-load 'evil
  (with-eval-after-load 'elpaca-ui (evil-make-intercept-map elpaca-ui-mode-map))
  (with-eval-after-load 'elpaca-info (evil-make-intercept-map elpaca-info-mode-map)))
