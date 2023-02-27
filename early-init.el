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


;; From doom emacs
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))



;;; Setup PATH
(cond 
 (IS-MAC
	(setenv "PATH" (concat "/opt/homebrew/bin" path-separator
												 "~/.cargo/bin" path-separator
												 "/Library/TeX/texbin" path-separator
												 "~/miniforge3/bin" path-separator
												 "~/.local/bin" path-separator
												 "/usr/local/bin" path-separator
												 (getenv "PATH"))))
 (IS-LINUX
	(setenv "PATH" (concat "~/.local/bin" path-separator
												 "~/.cargo/bin" path-separator
												 "/usr/local/bin" path-separator
												 (getenv "PATH")))))

;;; Setup exec-pat

(cond
 (IS-MAC
	(add-to-list 'exec-path "/opt/homebrew/bin")
	(add-to-list 'exec-path "~/.cargo/bin")
	(add-to-list 'exec-path "/Library/TeX/texbin")
	(add-to-list 'exec-path "~/miniforge3/bin")
	(add-to-list 'exec-path "~/.local/bin")
	(add-to-list 'exec-path "/usr/local/bin"))
 (IS-LINUX
	(add-to-list 'exec-path "~/.cargo/bin")
	(add-to-list 'exec-path "~/.local/bin")
	(add-to-list 'exec-path "~/micromamba/bin")
	(add-to-list 'exec-path "/usr/local/bin")))



(setq ring-bell-function 'ignore)



;; FIXME
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 100))
;; (set-face-attribute 'default (selected-frame) :height 200)

(provide 'early-init)
