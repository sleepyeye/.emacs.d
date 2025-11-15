;;; tree-sitter.el --- Tree-sitter configuration -*- lexical-binding: t; -*-

(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml"))

        major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (c-or-c++-mode   . c-or-c++-ts-mode)
          (cmake-mode      . cmake-ts-mode)
          (conf-toml-mode  . toml-ts-mode)
          (js-json-mode    . json-ts-mode)
          (markdown-mode   . markdown-ts-mode)
          (python-mode     . python-ts-mode)
          (sh-mode         . bash-ts-mode))

        treesit-font-lock-level 4)

  ;; Auto-mode-alist for files without existing major modes
  (add-to-list 'auto-mode-alist '("CMakeLists\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode)))

;; Leader keybindings for tree-sitter functions
(with-eval-after-load 'general
  (sleepy/leader-def
    "t"   '(:ignore t :which-key "Tree-sitter")
    "t i" '(treesit-inspect-mode :which-key "inspect mode")
    "t e" '(treesit-explore-mode :which-key "explore mode")
    "t q" '(treesit-query-builder :which-key "query builder")
    "t n" '(treesit-defun-name :which-key "defun name")
    "t g" '(treesit-install-language-grammar :which-key "install grammar")
    "t b" '(treesit-beginning-of-defun :which-key "defun begin")
    "t E" '(treesit-end-of-defun :which-key "defun end")
    "t t" '(treesit-transpose-sexps :which-key "transpose sexps")))

;;; tree-sitter.el ends here
