;;; tree-sitter.el --- Tree-sitter configuration -*- lexical-binding: t; -*-

(use-package treesit
  :ensure nil
  :defer t
  :init
  (add-hook 'after-init-hook
            (lambda ()
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
                      (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))
              (setq major-mode-remap-alist
                    '((c-mode          . c-ts-mode)
                      (c++-mode        . c++-ts-mode)
                      (c-or-c++-mode   . c-or-c++-ts-mode)
                      (cmake-mode      . cmake-ts-mode)
                      (conf-toml-mode  . toml-ts-mode)
                      (js-json-mode    . json-ts-mode)
                      (markdown-mode   . markdown-ts-mode)
                      (python-mode     . python-ts-mode)
                      (sh-mode         . bash-ts-mode)))
              (setq treesit-font-lock-level 4)
              ;; Auto-mode-alist for files without existing major modes
              (add-to-list 'auto-mode-alist '("CMakeLists\\'" . cmake-ts-mode))
              (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
              (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode)))))

;;; tree-sitter.el ends here
