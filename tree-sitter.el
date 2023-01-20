;;; remap tree-stitter supported modes
(push '(python-mode . python-ts-mode) major-mode-remap-alist)
(push '(css-mode . css-ts-mode) major-mode-remap-alist)
(push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
(push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
(push '(typescript-mode . typescript-ts-mode) major-mode-remap-alist)
(push '(c-mode . c-ts-mode) major-mode-remap-alist)
(push '(c++-mode . c++-ts-mode) major-mode-remap-alist)




;;; Emacs built-in treesitter package
(use-feature treesit
  :demand t
  :init
  :config
  (add-to-list 'treesit-language-source-alist
	       '(c "https://github.com/tree-sitter/tree-sitter-c.git"))
  (add-to-list 'treesit-language-source-alist
	       '(c++ "https://github.com/tree-sitter/tree-sitter-cpp.git"))
  (add-to-list 'treesit-language-source-alist
	       '(rust "https://github.com/tree-sitter/tree-sitter-rust.git"))
  (add-to-list 'treesit-language-source-alist
	       '(cmake "https://github.com/uyha/tree-sitter-cmake.git"))
  (add-to-list 'treesit-language-source-alist
	       '(python "https://github.com/tree-sitter/tree-sitter-python.git"))
     
  (setq treesit-font-lock-level 4))

(elpaca-use-package evil-textobj-tree-sitter
  :after (treest evil)
  :demand t
  :config

  ;; Goto start of next function
  (define-key evil-normal-state-map (kbd "]f") (lambda ()
						 (interactive)
						 (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  ;; Goto start of previous function
  (define-key evil-normal-state-map (kbd "[f") (lambda ()
						 (interactive)
						 (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  ;; Goto end of next function
  (define-key evil-normal-state-map (kbd "]F") (lambda ()
						 (interactive)
						 (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  ;; Goto end of previous function
  (define-key evil-normal-state-map (kbd "[F") (lambda ()
						 (interactive)
						 (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

