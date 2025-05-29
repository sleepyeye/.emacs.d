(use-package projectile
  :config
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "^\\build$")
  (add-to-list 'projectile-globally-ignored-directories "^\\elpa$")
  (add-to-list 'projectile-globally-ignored-directories "^\\url$")
  (add-to-list 'projectile-globally-ignored-directories "^\\__pycache__$")
  (setq projectile-project-search-path '("~/workspace/"))
  (setq projectile-enable-caching t)
  (setq projectile-require-project-root t)

  (defun open-project-todo ()
	"Open `todo.org` in the root of the current projectile project.
If the file doesn't exist, create it."
	(interactive)
	(let* ((project-root (projectile-project-root))
		   (todo-path (expand-file-name "todo.org" project-root)))
	  (unless (file-exists-p todo-path)
		(with-temp-buffer
		  (insert "#+TITLE: TODOs for " (file-name-nondirectory (directory-file-name project-root)) "\n\n")
		  (write-file todo-path)))
	  (find-file todo-path)))

  (sleepy/leader-def "p '" #'open-project-todo))

(use-package consult-projectile)
