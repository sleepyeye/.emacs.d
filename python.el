;;; python.el --- Python development configuration -*- lexical-binding: t; -*-

(defun sleepy/python-capf ()
  "Setup completion-at-point for Python mode."
  (when (derived-mode-p 'python-base-mode)
	(setq-local completion-at-point-functions
				(list (cape-capf-super
					   #'eglot-completion-at-point
					   #'cape-keyword)))))

(add-hook 'eglot-managed-mode-hook #'sleepy/python-capf)

(use-package exec-path-from-shell
  :if (memq (window-system) '(mac ns))
  :init
  (defvar sleepy/exec-path-from-shell-did-init nil)
  :config
  (unless sleepy/exec-path-from-shell-did-init
    (exec-path-from-shell-initialize)
    (setq sleepy/exec-path-from-shell-did-init t)))


;; Auto-formatting with apheleia
(with-eval-after-load 'apheleia
  ;; Configure ruff as Python formatter (fast, modern alternative to black+isort)
  (when (executable-find "ruff")
    (setf (alist-get 'ruff apheleia-formatters)
          '("ruff" "format" "--stdin-filename" filepath "-"))
    (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)))

(use-package pet
  :ensure t
  :init
  ;; 1) Exclude slow recursive search, use only fast paths
  (setq pet-find-file-functions
        '(pet-find-file-from-project-root
          pet-locate-dominating-file
          pet-find-file-from-project-root-natively))

  (when (executable-find "fd")
	(setq pet-fd-command "fd"
		  pet-fd-command-args '("-tf" "-cnever" "-H" "-a" "-g")))

  (defun sleepy/pet-fast-maybe-enable ()
    "Enable pet-mode only when clear project markers exist in Python buffer."
    (when (and (derived-mode-p 'python-base-mode)
               (or (locate-dominating-file default-directory ".venv")
                   (locate-dominating-file default-directory "pyproject.toml")
                   (locate-dominating-file default-directory "environment.yml")))
      (pet-mode 1)))


  ;; [optional] external deps for toml, yaml
  (when (executable-find "dasel")
    (setq pet-toml-to-json-program "dasel"
          pet-toml-to-json-program-arguments '("-"))
    (setq pet-yaml-to-json-program "dasel"
          pet-yaml-to-json-program-arguments '("-o=json")))
  :config
  (add-hook 'python-base-mode-hook #'sleepy/pet-fast-maybe-enable -10))


;;; --- Run current Python file asynchronously (uv-aware) ---
(require 'ansi-color)
(require 'compile)

(defgroup sleepy-python-run nil
  "Async runner for Python buffers."
  :group 'tools
  :prefix "sleepy/python-")

(defcustom sleepy/python-run-buffer-name "*Python Run*"
  "Compilation buffer name for async Python runs."
  :type 'string)

(defun sleepy/python--project-root ()
  "Return project root or DEFAULT-DIRECTORY."
  (or (when (fboundp 'project-root)
        (ignore-errors (project-root (project-current))))
      (locate-dominating-file default-directory ".git")
      (locate-dominating-file default-directory "pyproject.toml")
      default-directory))

(defun sleepy/python--interpreter ()
  "Pick python interpreter: prefer buffer-local, else 'python3'/'python'."
  (or (and (boundp 'python-shell-interpreter)
           (stringp python-shell-interpreter)
           (not (string-empty-p python-shell-interpreter))
           python-shell-interpreter)
      (executable-find "python3")
      (executable-find "python")
      "python"))

(defun sleepy/python--uv-available-p ()
  (and (executable-find "uv")
       (locate-dominating-file default-directory "pyproject.toml")))

(defun sleepy/python--build-cmd (file extra-args)
  "Build a shell command to run FILE with EXTRA-ARGS list."
  (let* ((py (sleepy/python--interpreter))
         (quoted-file (shell-quote-argument (expand-file-name file)))
         (args-str (mapconcat #'shell-quote-argument extra-args " ")))
    (if (sleepy/python--uv-available-p)
        ;; For uv projects, run based on .venv/lock
        (string-join (delq nil
                           (list "uv" "run" py "-u" quoted-file
                                 (unless (string-empty-p args-str) args-str)))
                     " ")
      ;; Regular .venv/conda or system python (buffer/project env priority)
      (string-join (delq nil
                         (list py "-u" quoted-file
                               (unless (string-empty-p args-str) args-str)))
                   " "))))

;; Handle color ANSI codes (compilation buffer)
(defun sleepy/python--colorize-compilation ()
  (when (eq (current-buffer)
            (get-buffer sleepy/python-run-buffer-name))
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(add-hook 'compilation-filter-hook #'sleepy/python--colorize-compilation)

(defun sleepy/python-run-file-async (&optional prefix)
  "Run current Python file asynchronously in a compilation buffer.
With PREFIX (C-u), prompt for extra argv."
  (interactive "P")
  (unless buffer-file-name
    (user-error "This buffer is not a file"))
  (save-buffer)
  (let* ((default-directory (sleepy/python--project-root))
         (extra (if prefix
                    (split-string (read-string "Args: ") " " t)
                  nil))
         (cmd (sleepy/python--build-cmd buffer-file-name extra)))
    ;; non-blocking run; reuse named buffer
    (compilation-start cmd 'compilation-mode
                       (lambda (_) sleepy/python-run-buffer-name))))

(defun sleepy/python-rerun-last ()
  "Re-run the last Python run command in the same buffer."
  (interactive)
  (let ((buf (get-buffer sleepy/python-run-buffer-name)))
    (unless buf
      (user-error "No previous run buffer exists"))
    (with-current-buffer buf
      (recompile))))

;; --- keybindings via general (consistent) ---
(with-eval-after-load 'general
  ;; C-c bindings (common for python-mode / python-ts-mode)
  (general-define-key
   :keymaps '(python-mode-map python-ts-mode-map)
   "C-c C-c" #'sleepy/python-run-file-async
   "C-c C-r" #'sleepy/python-rerun-last))

;; Enable apheleia for Python modes
(add-hook 'python-base-mode-hook #'apheleia-mode)

;;; python.el ends here
