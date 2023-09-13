;; Early init settings
(setq package-enable-at-startup nil
      inhibit-default-init nil
      native-comp-async-report-warnings-errors nil
      ring-bell-function 'ignore
      inhibit-startup-screen t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Garbage Collection
(setq gc-cons-threshold 1073741824
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024))

;; System detection
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

;; PATH and exec-path
(cond
  (IS-MAC
    (setenv "PATH" (concat "/opt/homebrew/bin" path-separator
                           (expand-file-name "~/.cargo/bin") path-separator
                           "/Library/TeX/texbin" path-separator
                           (expand-file-name "~/miniforge3/bin") path-separator
                           (expand-file-name "~/.local/bin") path-separator
                           "/usr/local/bin" path-separator
                           (getenv "PATH")))
    (add-to-list 'exec-path "/opt/homebrew/bin")
    (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
    (add-to-list 'exec-path "/Library/TeX/texbin")
    (add-to-list 'exec-path (expand-file-name "~/miniforge3/bin"))
    (add-to-list 'exec-path (expand-file-name "~/.local/bin"))
    (add-to-list 'exec-path "/usr/local/bin"))
  (IS-LINUX
    (setenv "PATH" (concat "~/.local/bin" path-separator
                           "~/.cargo/bin" path-separator
                           (expand-file-name "~/miniconda3/bin") path-separator
                           "/usr/local/bin" path-separator
                           "/usr/local/sbin" path-separator
                           "/usr/sbin" path-separator
                           "/sbin" path-separator
                           (getenv "PATH")))
    (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
    (add-to-list 'exec-path (expand-file-name"~/.local/bin"))
    (add-to-list 'exec-path (expand-file-name"~/miniconda3/bin"))
    (add-to-list 'exec-path "/usr/local/bin")
    (add-to-list 'exec-path "/usr/local/sbin")
    (add-to-list 'exec-path "/usr/sbin")
    (add-to-list 'exec-path "/sbin")))


(cond
 (IS-MAC
  ;; make title bar transparent and apply dark color
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))))

;; Miscellaneous optimizations
(setq idle-update-delay 1.0
      bidi-display-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil
      fast-but-imprecise-scrolling t
      inhibit-compacting-font-caches t)

;; Frame dimensions
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 100))

;; Disable x-apply-session-resources
(advice-add #'x-apply-session-resources :override #'ignore)


;; Load elpaca
(defvar user-emacs-directory "~/.emacs.d")
(load (concat user-emacs-directory "bootstraps"))
(elpaca-wait)

(with-eval-after-load 'evil
  (with-eval-after-load 'elpaca-ui (evil-make-intercept-map elpaca-ui-mode-map))
  (with-eval-after-load 'elpaca-info (evil-make-intercept-map elpaca-info-mode-map)))
