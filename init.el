(defvar user-emacs-directory "~/.emacs.d")
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil
			      :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "repos/elpaca/" elpaca-directory))
	   (build (expand-file-name "elpaca/" elpaca-builds-directory))
	   (order (cdr elpaca-order))
	   ((add-to-list 'load-path (if (file-exists-p build) build repo)))
	   ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
	       ((zerop (call-process "git" nil buffer t "clone"
				     (plist-get order :repo) repo)))
	       (default-directory repo)
	       ((zerop (call-process "git" nil buffer t "checkout"
				     (or (plist-get order :ref) "--")))))
	  (progn
	    (byte-recompile-directory repo 0 'force)
	    (require 'elpaca)
	    (and (fboundp 'elpaca-generate-autoloads)
		 (elpaca-generate-autoloads "elpaca" repo))
	    (kill-buffer buffer))
	(error "%s" (with-current-buffer buffer (buffer-string))))
    ((error)
     (warn "%s" err)
     (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca use-package (require 'use-package))

;;; for built-in features
;;; stolen from @progfolio's emacs config
(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(elpaca nil (use-package ,name
		 :ensure nil
		 ,@args)))

(setenv "PATH" (concat "/opt/homebrew/bin" path-separator
		       "~/.cargo/bin" path-separator
		       "/Library/TeX/texbin" path-separator
		       (getenv "PATH")))


;; From doom emacs
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (memq system-type '(darwin berkeley-unix gnu/kfreebsd)))

(cond
 (IS-MAC
  ;; mac-* variables are used by the special emacs-mac build of Emacs by
  ;; Yamamoto Mitsuharu, while other builds use ns-*.
  (setq mac-command-modifier      'super
	ns-command-modifier       'super
	mac-option-modifier       'meta
	ns-option-modifier        'meta
	;; Free up the right option for character composition
	mac-right-option-modifier 'none
	ns-right-option-modifier  'none))
 (IS-WINDOWS
  (setq w32-lwindow-modifier 'super
	w32-rwindow-modifier 'super)))

(require 'xref)

;;; load core packages
(load "~/.emacs.d/evil.el")
(load "~/.emacs.d/general.el")
(load "~/.emacs.d/which-key.el")
(load "~/.emacs.d/projectile.el")
(load "~/.emacs.d/completion.el")
(load "~/.emacs.d/lsp.el")
(load "~/.emacs.d/font.el")
(load "~/.emacs.d/ui.el")
(load "~/.emacs.d/magit.el")
(load "~/.emacs.d/tree-sitter.el")
(load "~/.emacs.d/dired.el")

;;; setup languages
(load "~/.emacs.d/tex.el")


;; FIXME
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 100))
(set-face-attribute 'default (selected-frame) :height 200)
(recentf-mode 1)
(global-hl-line-mode +1)
