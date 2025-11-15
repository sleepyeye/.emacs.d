;;; search.el --- ripgrep / dumb-jump setup -*- lexical-binding: t; -*-

;; dumb-jump (use as xref backend)
(use-package dumb-jump
  :commands dumb-jump-xref-activate
  :init
  ;; Prefer ripgrep (faster)
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-force-searcher  'rg)
  :config
  ;; Register as xref backend → integrates with default xref UI/keys
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;;; --- Imenu: only classes & functions ---------------------------------

;; Show hierarchy in one line in consult-imenu (e.g., Class.method)
(with-eval-after-load 'consult
  (setq consult-imenu-namespace 'concat))

;; Common: auto-rescan and accuracy options
(setq imenu-auto-rescan t
	  imenu-use-markers t)

;;; --- Advanced ripgrep search functions -----------------------------------

;; Search for TODO/FIXME/HACK comments
(defun sleepy/search-todos ()
  "Search for TODO, FIXME, HACK, XXX, NOTE in project."
  (interactive)
  (consult-ripgrep nil "TODO|FIXME|HACK|XXX|NOTE"))

;; Search for Python function/class definitions
(defun sleepy/search-python-defs ()
  "Search for Python function/class definitions."
  (interactive)
  (consult-ripgrep nil "^(def|class) \\w+"))

;; Search for LaTeX sections
(defun sleepy/search-latex-sections ()
  "Search for LaTeX sections/chapters."
  (interactive)
  (consult-ripgrep nil "\\\\(chapter|section|subsection|subsubsection)\\{"))

;; Search for symbol at point in project
(defun sleepy/search-symbol-at-point ()
  "Search for symbol at point in project."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (if symbol
        (consult-ripgrep nil symbol)
      (message "No symbol at point"))))

;; Search only in Python files
(defun sleepy/search-in-python ()
  "Search only in Python files."
  (interactive)
  (let* ((pattern (read-string "Search in Python files: "))
         (consult-ripgrep-args (concat consult-ripgrep-args " -t py")))
    (consult-ripgrep nil pattern)))

;; Search only in LaTeX files
(defun sleepy/search-in-latex ()
  "Search only in LaTeX files."
  (interactive)
  (let* ((pattern (read-string "Search in LaTeX files: "))
         (consult-ripgrep-args (concat consult-ripgrep-args " -t tex")))
    (consult-ripgrep nil pattern)))

;; Search only in C/C++ files
(defun sleepy/search-in-cpp ()
  "Search only in C/C++ files."
  (interactive)
  (let* ((pattern (read-string "Search in C/C++ files: "))
         (consult-ripgrep-args (concat consult-ripgrep-args " -t cpp -t c")))
    (consult-ripgrep nil pattern)))

;; Search with file type exclusions
(defun sleepy/search-exclude-tests ()
  "Search excluding test files."
  (interactive)
  (let* ((pattern (read-string "Search (exclude tests): "))
         (consult-ripgrep-args (concat consult-ripgrep-args " -g '!*test*.py' -g '!test_*.py'")))
    (consult-ripgrep nil pattern)))

;; Keybindings for search functions
(with-eval-after-load 'general
  (when (fboundp 'sleepy/leader-def)
    (sleepy/leader-def
      "s t" '(sleepy/search-todos :which-key "search TODOs")
      "s y" '(sleepy/search-symbol-at-point :which-key "search symbol")
      "s P" '(sleepy/search-in-python :which-key "search in Python")
      "s L" '(sleepy/search-in-latex :which-key "search in LaTeX")
      "s C" '(sleepy/search-in-cpp :which-key "search in C/C++")
      "s x" '(sleepy/search-exclude-tests :which-key "search exclude tests"))))

;;; search.el ends here
