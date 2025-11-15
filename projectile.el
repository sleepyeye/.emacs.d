;;; projectile.el --- lean, git-root only, standard keybinds -*- lexical-binding: t; -*-

(use-package projectile
  :ensure t
  :init
  ;; Only recognize Git root as project (prevent unwanted parent folder auto-registration)
  (setq projectile-project-root-files-bottom-up '(".git")
        projectile-project-root-files-top-down nil
        projectile-project-root-files-top-down-recurring nil)

  ;; Don't automatically add to known-projects when opening files
  (setq projectile-track-known-projects-automatically nil)

  ;; Projectile commands don't work when not in project root
  (setq projectile-require-project-root t)

  ;; Search path/depth: Only 2 levels below ~/workspace
  (setq projectile-project-search-path '(("~/workspace" . 2)))

  ;; Explicit cache directory (in ~/.emacs.d/ instead of home folder)
  (setq projectile-cache-file
        (expand-file-name "projectile.cache" user-emacs-directory)
        projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

  ;; Don't recognize directories outside ~/workspace as projects
  ;; (Exception: ~/.emacs.d is allowed)
  (setq projectile-ignored-projects
        (list "~/" "/tmp/" "/usr/" "/opt/" "/etc/"
              (expand-file-name "~/.config/")))

  ;; Only allow ~/workspace or ~/.emacs.d as projects
  (setq projectile-ignored-project-function
        (lambda (project-root)
          (let ((workspace-path (expand-file-name "~/workspace/"))
                (emacs-path (expand-file-name "~/.emacs.d/")))
            ;; Ignore if not under ~/workspace/ or ~/.emacs.d/
            (not (or (string-prefix-p workspace-path project-root)
                     (string-prefix-p emacs-path project-root))))))

  ;; Indexing/cache: Generally the fastest combination
  (setq projectile-indexing-method 'alien
        projectile-enable-caching t)

  ;; Completion system: Emacs default (no conflict with vertico/orderless)
  (setq projectile-completion-system 'default)

  ;; Use ripgrep for file listing commands if available
  (when (executable-find "rg")
    (setq projectile-generic-command "rg --files --hidden --follow --color=never -0"
          projectile-git-command     "rg --files --hidden --follow --color=never -z"))

  :config
  (projectile-mode 1)

  ;; Globally ignored directories (by name) - safer than regex
  (dolist (name '("build" "dist" "target" "node_modules" "__pycache__"
                  ".venv" ".direnv" "elpa" "url"))
    (add-to-list 'projectile-globally-ignored-directories name))

  ;; Project switch action (default is dired). Change to find-file if preferred:
  ;; (setq projectile-switch-project-action #'projectile-find-file)

  ;; Convenience function: Open or create todo.org at project root
  (defun sleepy/open-project-todo ()
    "Open or create `todo.org` at current Projectile project root.

This function:
- Validates that you are in a Projectile project
- Creates todo.org with a proper title if it doesn't exist
- Opens the file in the current window

The todo.org file is created at the project root directory."
    (interactive)
    (condition-case err
        (let ((root (projectile-project-root)))
          (unless root
            (user-error "Not in a Projectile project"))
          (let ((path (expand-file-name "todo.org" root)))
            (unless (file-exists-p path)
              (with-temp-buffer
                (insert "#+TITLE: TODOs for "
                        (file-name-nondirectory (directory-file-name root)) "\n\n")
                (write-file path)))
            (find-file path)))
      (error
       (user-error "Failed to open project todo: %s" (error-message-string err)))))

  ;; Add 't' to Commander: M-x projectile-commander → t
  (add-to-list 'projectile-commander-methods
               '(?t "Open or create todo.org at project root" sleepy/open-project-todo))

  ;; Standard keybindings (C-c p ...) - only the most frequently used
  (let ((map projectile-command-map))
    (define-key map (kbd "p") #'projectile-switch-project)
    (define-key map (kbd "f") #'projectile-find-file)
    (define-key map (kbd "d") #'projectile-find-dir)
    (define-key map (kbd "b") #'projectile-switch-to-buffer)
    (define-key map (kbd "k") #'projectile-kill-buffers)
    (define-key map (kbd "r") #'projectile-recentf)
    (define-key map (kbd "s r") #'projectile-ripgrep)
    (define-key map (kbd ":") #'projectile-commander)
    (define-key map (kbd "'") #'sleepy/open-project-todo))

  ;; Leader key integration (if available): SPC p → projectile-command-map
  (when (fboundp 'sleepy/leader-def)
    (sleepy/leader-def "p" '(projectile-command-map :which-key "Project"))))

(use-package consult-projectile
  :after (projectile consult)
  :ensure t)

;;; projectile.el ends here
