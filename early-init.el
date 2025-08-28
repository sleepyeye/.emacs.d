;;; early-init.el --- early-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

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
      scroll-bar-mode nil
      tool-bar-mode nil
      frame-inhibit-implied-resize t)

;; Performance and Optimization
(setq idle-update-delay 1.0
      bidi-display-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil
      fast-but-imprecise-scrolling t
      inhibit-compacting-font-caches t
      load-prefer-newer t
      read-process-output-max (* 4 1024 1024))

;; UI Tweaks
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Frame Size
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 100))

;; System Type Detection
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

;; PATH and exec-path Setup
(cond
 ((and IS-MAC)
  (setenv "PATH" (concat "/opt/homebrew/bin" path-separator
                         (expand-file-name "~/.cargo/bin") path-separator
                         "/Library/TeX/texbin" path-separator
                         (expand-file-name "~/miniforge3/bin") path-separator
                         (expand-file-name "~/.local/bin") path-separator
                         "/usr/local/bin" path-separator
                         (getenv "PATH")))
  (dolist (path '("/opt/homebrew/bin"
                  "~/.cargo/bin"
                  "/Library/TeX/texbin"
                  "~/miniforge3/bin"
                  "~/.local/bin"
                  "/usr/local/bin"))
    (add-to-list 'exec-path (expand-file-name path))))

 ((and IS-LINUX)
  (setenv "PATH" (concat (expand-file-name "~/.local/bin") path-separator
                         (expand-file-name "~/.cargo/bin") path-separator
                         (expand-file-name "~/miniconda3/bin") path-separator
                         "/usr/local/bin" path-separator
                         "/usr/local/sbin" path-separator
                         "/usr/sbin" path-separator
                         "/sbin" path-separator
                         (getenv "PATH")))
  (dolist (path '("~/.cargo/bin"
                  "~/.local/bin"
                  "~/miniconda3/bin"
                  "/usr/local/bin"
                  "/usr/local/sbin"
                  "/usr/sbin"
                  "/sbin"))
    (add-to-list 'exec-path (expand-file-name path)))))

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

;; evil keybinding setup for elpaca
(with-eval-after-load 'evil
  (with-eval-after-load 'elpaca-ui (evil-make-intercept-map elpaca-ui-mode-map))
  (with-eval-after-load 'elpaca-info (evil-make-intercept-map elpaca-info-mode-map)))
